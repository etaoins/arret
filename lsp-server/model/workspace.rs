#[derive(Debug)]
pub struct Workspace {
    _name: String,
}

impl Workspace {
    pub fn new(name: String) -> Workspace {
        Workspace { _name: name }
    }
}
