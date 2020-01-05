pub const TYPE_ONLY_PREFIX: &str = "/type ";
pub const QUIT_COMMAND: &str = "/quit";
pub const HELP_COMMAND: &str = "/help";

pub enum ParsedCommand {
    EvalValue(String),
    EvalType(String),
    Quit,
    Other,
}

pub fn parse_command(mut line: String) -> ParsedCommand {
    match line.as_ref() {
        _ if line.starts_with(TYPE_ONLY_PREFIX) => {
            line.drain(0..TYPE_ONLY_PREFIX.len());
            ParsedCommand::EvalType(line)
        }
        HELP_COMMAND => {
            println!("Available REPL commands:");
            println!();
            println!("/help                 Prints this summary");
            println!("/type <expression>    Evaluates the type of the given expression");
            println!("/quit                 Exits the REPL");
            ParsedCommand::Other
        }
        QUIT_COMMAND => ParsedCommand::Quit,
        _ => ParsedCommand::EvalValue(line),
    }
}
