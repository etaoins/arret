use std::sync::Arc;

pub struct Document {
    _version: Option<i64>,
    _text: Arc<str>,
}

impl Document {
    pub fn new(version: Option<i64>, text: Arc<str>) -> Document {
        Document {
            _version: version,
            _text: text,
        }
    }
}
