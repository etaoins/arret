#[derive(PartialEq, Debug, Hash, Clone)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

pub fn t2s(v: &str) -> Span {
    let lo = v.find('^').unwrap_or(v.len()) as u32;
    let hi = v.rfind('^').map(|i| i + 1).unwrap_or(v.len()) as u32;

    Span { lo, hi }
}
