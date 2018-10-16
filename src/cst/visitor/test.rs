use std::mem;

use super::Visitor;
use cst::{BinaryOp, Expr, Literal, Number};

macro_rules! boxed {
    ($e:expr) => {
        Box::new($e)
    };
}

macro_rules! binop {
    ($e1:expr, $o:expr, $e2:expr) => {
        Expr::Binary(boxed!($e1), $o, boxed!($e2))
    };
}

macro_rules! int {
    ($i:expr) => {
        Expr::Literal(Literal::Number(Number::Integer($i)))
    };
}

macro_rules! float {
    ($f:expr) => {
        Expr::Literal(Literal::Number(Number::Float($f)))
    };
}

#[test]
fn recursively_count_binops() {
    #[derive(Default)]
    struct Counts {
        and: u32,
        eq: u32,
        like: u32,
        gt: u32,
    }

    impl Visitor for Counts {
        fn visit_binary_op(&mut self, op: &mut BinaryOp) {
            match op {
                BinaryOp::And => self.and += 1,
                BinaryOp::Eq => self.eq += 1,
                BinaryOp::Like => self.like += 1,
                BinaryOp::Gt => self.gt += 1,
                _ => {}
            }
        }
    }

    // (1 like (2 xor 3)) and ((((4 eq 5) eq 6) eq 7) like 8)
    let mut expr = binop! {
        binop! {
            int!(1),
            BinaryOp::Like,
            binop! {
                int!(2),
                BinaryOp::Xor,
                int!(3)
            }
        },
        BinaryOp::And,
        binop! {
            binop! {
                binop! {
                    binop! {
                        int!(4),
                        BinaryOp::Eq,
                        int!(5)
                    },
                    BinaryOp::Eq,
                    int!(6)
                },
                BinaryOp::Eq,
                int!(7)
            },
            BinaryOp::Like,
            int!(8)
        }
    };

    let mut counts = Counts::default();
    counts.visit_expr(&mut expr);

    assert_eq!(1, counts.and);
    assert_eq!(3, counts.eq);
    assert_eq!(2, counts.like);
    assert_eq!(0, counts.gt);
}

#[test]
fn transform_binops() {
    struct Converter;

    impl Visitor for Converter {
        fn visit_number(&mut self, number: &mut Number) {
            let i = match number {
                Number::Integer(i) => *i,
                Number::Float(_) => return,
            };

            mem::replace(number, Number::Float(i as f64));
        }

        fn visit_binary_op(&mut self, op: &mut BinaryOp) {
            mem::replace(op, BinaryOp::Like);
        }
    }

    // 2 xor (3 xor 4)
    let mut actual = binop! {
        int!(2),
        BinaryOp::Xor,
        binop! {
            int!(3),
            BinaryOp::Xor,
            int!(4)
        }
    };

    // 2.0 like (3.0 like 4.0)
    let expected = binop! {
        float!(2.0),
        BinaryOp::Like,
        binop! {
            float!(3.0),
            BinaryOp::Like,
            float!(4.0)
        }
    };

    Converter.visit_expr(&mut actual);
    assert_eq!(expected, actual);
}
