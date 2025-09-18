#[macro_use]
extern crate slog;
#[macro_use]
extern crate slog_scope;
extern crate slog_async;
extern crate slog_term;

use std::{
    error::Error,
    fmt::Display,
    io,
    ops::{Not, Range},
    path::Path,
    process::ExitCode,
};

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{error::Rich, input::Input};
use clap::{Args, Parser, Subcommand};
use nanorust::{
    interpret::{FuncOverride, InterpretEnv},
    ir::{IR, RValue, RValueInner},
    lexer::Token,
    span::Spanned,
};
use slog::Drain;
use yansi::Paint;

type Result<T> = std::result::Result<T, Box<dyn Error>>;

#[derive(Debug, Parser)]
#[clap(name = "nanorust", version)]
struct Cli {
    #[clap(flatten)]
    cli_args: CliArgs,

    #[clap(subcommand)]
    cli_command: CliCommand,
}

#[derive(Debug, Args)]
struct CliArgs {
    /// Verbosity level
    #[clap(long, short, global = true, action = clap::ArgAction::Count)]
    verbose: u8,
}

#[derive(Debug, Subcommand)]
enum CliCommand {
    /// Interpret a file written in nanorust language
    Interpret(InterpretCmd),
}

impl CliCommand {
    fn name(&self) -> &str {
        match self {
            CliCommand::Interpret(_) => "interpret",
        }
    }
}

#[derive(Debug, Args)]
struct InterpretCmd {
    /// Disable type checking before interpreting
    #[clap(long, short = 'T')]
    untyped: bool,

    /// Path to nanorust language source file to interpret
    path: String,
}

fn plural(n: usize, text: impl AsRef<str>) -> String {
    match n {
        1 => format!("{} {}", n, text.as_ref()),
        _ => format!("{} {}s", n, text.as_ref()),
    }
}

fn init_global_logger(args: &CliArgs) -> slog_scope::GlobalLoggerGuard {
    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::FullFormat::new(decorator).build().fuse();
    let drain = slog::LevelFilter(
        drain,
        match &args.verbose {
            0 => slog::Level::Warning,
            1 => slog::Level::Info,
            2 => slog::Level::Debug,
            _ => slog::Level::Trace,
        },
    )
    .fuse();
    let drain = std::sync::Mutex::new(drain).fuse();
    let log = slog::Logger::root(drain, o!());
    slog_scope::set_global_logger(log)
}

fn main() -> ExitCode {
    let args = Cli::parse();
    let _guard = init_global_logger(&args.cli_args);

    trace!("current subcommand: {}", args.cli_command.name());
    let res = match args.cli_command {
        CliCommand::Interpret(interpret_args) => interpret(interpret_args),
    };
    match res {
        Ok(code) => code,
        Err(error) => {
            eprintln!("{} {}", "Error:".red(), error);
            ExitCode::FAILURE
        }
    }
}

enum InterpretResult<'a> {
    Value(String),
    Report(Vec<Report<'static, (&'a str, Range<usize>)>>),
}

fn build_error_reports<'a, 'src, T: Display>(
    name: &'a str,
    errors: Vec<Rich<'src, T>>,
) -> Vec<Report<'static, (&'a str, Range<usize>)>> {
    errors
        .into_iter()
        .map(|e| {
            Report::build(ReportKind::Error, (name, e.span().into_range()))
                .with_message(e.to_string())
                .with_label(
                    Label::new((name, e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new((name, span.into_range()))
                        .with_message(format!("while parsing this {}", label))
                        .with_color(Color::Yellow)
                }))
                .with_code("E00")
                .finish()
        })
        .collect()
}

fn parse_into_tokens<'a, 'src>(
    source: &'src str,
    name: &'a str,
) -> std::result::Result<Vec<Spanned<Token<'src>>>, InterpretResult<'a>> {
    let (tokens, lexing_errors) =
        chumsky::Parser::parse(&nanorust::lexer::lexer(), source).into_output_errors();
    if !lexing_errors.is_empty() {
        // TODO: save errors and attempt to parse partial tokens
        error!(
            "found {} while parsing tokens for {}",
            plural(lexing_errors.len(), "error"),
            name
        );
        return Err(InterpretResult::Report(build_error_reports(
            name,
            lexing_errors,
        )));
    }
    let tokens = tokens.unwrap();
    info!(
        "successfully parsed {} for {}",
        plural(tokens.len(), "token"),
        name
    );
    Ok(tokens)
}

fn unary<T, R>(f: impl Fn(T) -> R + 'static) -> FuncOverride
where
    T: RValueInner,
    R: RValueInner,
{
    Box::new(move |args| {
        let arg = T::from_rvalue(args[0].clone())
            .ok_or_else(|| format!("expected `{}` as an argument", T::type_name()))?;
        Ok(f(arg).into_rvalue())
    })
}

fn binary<A, B, R>(f: impl Fn(A, B) -> R + 'static) -> FuncOverride
where
    A: RValueInner,
    B: RValueInner,
    R: RValueInner,
{
    Box::new(move |args| {
        let left = A::from_rvalue(args[0].clone())
            .ok_or_else(|| format!("expected `{}` as a first operand", A::type_name()))?;
        let right = B::from_rvalue(args[1].clone())
            .ok_or_else(|| format!("expected `{}` as a second operand", B::type_name()))?;
        Ok(f(left, right).into_rvalue())
    })
}

fn binary_ref<A, B, R>(f: impl Fn(&A, &B) -> R + 'static) -> FuncOverride
where
    A: RValueInner,
    B: RValueInner,
    R: RValueInner,
{
    Box::new(move |args| {
        let left = A::from_rvalue(args[0].clone())
            .ok_or_else(|| format!("expected `{}` as a first operand", A::type_name()))?;
        let right = B::from_rvalue(args[1].clone())
            .ok_or_else(|| format!("expected `{}` as a second operand", B::type_name()))?;
        Ok(f(&left, &right).into_rvalue())
    })
}

fn interpret_string<'src>(
    source: &'src str,
    name: &'static str,
) -> Result<InterpretResult<'static>> {
    let prelude_source = include_str!("../data/prelude.nrs");
    let prelude_tokens = match parse_into_tokens(prelude_source, "prelude.nrs") {
        Ok(tokens) => tokens,
        Err(error) => return Ok(error),
    };
    let tokens = match parse_into_tokens(source, name) {
        Ok(tokens) => tokens,
        Err(error) => return Ok(error),
    };
    let tokens = [prelude_tokens, tokens]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();

    let (ast, parsing_errors) = chumsky::Parser::parse(
        &nanorust::parser::parse_stmt(),
        tokens
            .as_slice()
            .map((source.len()..source.len()).into(), |x| (&x.0, &x.1)),
    )
    .into_output_errors();
    if !parsing_errors.is_empty() {
        error!(
            "found {} while parsing AST for {}",
            plural(parsing_errors.len(), "error"),
            name
        );
        return Ok(InterpretResult::Report(build_error_reports(
            name,
            parsing_errors,
        )));
    }
    let ast = ast.unwrap();
    info!("sucessfully produced AST for {}", name);
    debug!("AST: {:?}", ast);

    let ir: IR<'_> = match IR::from_ast(&ast) {
        Ok(ir) => ir,
        Err(error) => {
            return Ok(InterpretResult::Report(vec![
                error
                    .report(name)
                    .with_code(format!("E{:02}", error.code()))
                    .finish(),
            ]));
        }
    };
    debug!("IR: {:?}", ir.root());

    let mut env = InterpretEnv::new();
    env.register_builtins(
        ir.function_table(),
        [
            ("__add", binary(i32::wrapping_add)),
            ("__sub", binary(i32::wrapping_sub)),
            ("__mul", binary(i32::wrapping_mul)),
            ("__div", binary(i32::wrapping_div)),
            ("__rem", binary(i32::wrapping_rem)),
            ("__eq", binary_ref(i32::eq)),
            ("__ne", binary_ref(i32::ne)),
            ("__le", binary_ref(i32::le)),
            ("__ge", binary_ref(i32::ge)),
            ("__lt", binary_ref(i32::lt)),
            ("__gt", binary_ref(i32::gt)),
            ("__and", binary(|a, b| a && b)),
            ("__or", binary(|a, b| a || b)),
            ("__or", binary(|a, b| a || b)),
            ("__neg", unary(i32::wrapping_neg)),
            ("__not", unary(bool::not)),
            (
                "println",
                Box::new(|args: &[RValue]| {
                    println!("{:?}", args[0]);
                    Ok(RValue::default())
                }),
            ),
        ]
        .into(),
    );

    let result = env.interpret(&ir);

    Ok(InterpretResult::Value(format!(
        "{}",
        match result {
            Ok(value) => format!("{:?}", value),
            Err(error) => error.message().into(),
        }
    )))
}

fn interpret(args: InterpretCmd) -> Result<ExitCode> {
    let file_path = args.path.leak();
    info!("reading file {}", file_path);
    let source = std::fs::read_to_string(Path::new(file_path))?;
    let res = interpret_string(source.as_str(), file_path)?;
    Ok(match res {
        InterpretResult::Value(value) => {
            println!("{}", value);
            ExitCode::SUCCESS
        }
        InterpretResult::Report(reports) => {
            reports
                .into_iter()
                .map(|r| r.eprint((&*file_path, Source::from(source.as_str()))))
                .collect::<io::Result<()>>()?;
            ExitCode::FAILURE
        }
    })
}
