use runtime::abitype;

pub trait FunABI {
    fn takes_task(&self) -> bool;
    fn params(&self) -> &[abitype::ABIType];
    fn has_rest(&self) -> bool;
    fn ret(&self) -> &abitype::RetABIType;
}
