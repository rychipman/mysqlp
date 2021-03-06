use std::ops::DerefMut;

#[cfg(test)]
mod test;

use super::{
    AliasedColumn, BinaryOp, ColumnName, DerivedTable, Expr, Join, JoinPredicate, Limit, Literal,
    Number, Projects, Select, Statement, Table, TableName, UnaryOp,
};

pub trait Visitor {
    fn visit_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Binary(ref mut e1, ref mut op, ref mut e2) => {
                self.visit_expr(e1.as_mut());
                self.visit_binary_op(op);
                self.visit_expr(e2.as_mut());
            }
            Expr::Unary(ref mut op, ref mut e) => {
                self.visit_unary_op(op);
                self.visit_expr(e);
            }
            Expr::Literal(ref mut literal) => self.visit_literal(literal),
            Expr::Column(ref mut name) => {
                self.visit_column_name(name);
            }
            Expr::ScalarFunc(ref mut name, ref mut args) => self.visit_scalar_func(name, args),
            Expr::AggFunc(ref mut name, ref mut args) => self.visit_agg_func(name, args),
            Expr::Case => self.visit_case(),
        }
    }

    fn visit_unary_op(&mut self, _op: &mut UnaryOp) {}

    fn visit_binary_op(&mut self, _op: &mut BinaryOp) {}

    fn visit_literal(&mut self, literal: &mut Literal) {
        match literal {
            Literal::Number(ref mut number) => self.visit_number(number),
            Literal::Boolean(_) | Literal::Date() | Literal::String(_) | Literal::Null => {}
        }
    }

    fn visit_number(&mut self, _number: &mut Number) {}

    fn visit_limit(&mut self, limit: &mut Limit) {
        self.visit_expr(&mut limit.count);

        if let Some(ref mut offset) = limit.offset {
            self.visit_expr(offset);
        }
    }

    fn visit_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::Select(ref mut select) => self.visit_select(select),
        }
    }

    fn visit_select(&mut self, select: &mut Select) {
        self.visit_projects(&mut select.projects);

        if let Some(ref mut limit) = select.limit {
            self.visit_limit(limit);
        }

        if let Some(ref mut table) = select.table {
            self.visit_table(table);
        }
    }

    fn visit_table(&mut self, table: &mut Table) {
        match table {
            Table::Name(ref mut name) => self.visit_table_name(name),
            Table::Derived(ref mut derived_table) => self.visit_derived_table(derived_table),
            Table::Join(ref mut join) => self.visit_join(join),
        }
    }

    fn visit_join(&mut self, join: &mut Join) {
        self.visit_table(join.left.deref_mut());
        self.visit_table(join.right.deref_mut());

        if let Some(ref mut pred) = join.predicate {
            self.visit_join_predicate(pred);
        }
    }

    fn visit_join_predicate(&mut self, predicate: &mut JoinPredicate) {
        match predicate {
            JoinPredicate::On(ref mut expr) => self.visit_expr(expr),
            JoinPredicate::Using(ref mut column_name) => {
                for mut name in column_name {
                    self.visit_column_name(name);
                }
            }
        }
    }

    fn visit_column_name(&mut self, _name: &mut ColumnName) {}

    fn visit_table_name(&mut self, _name: &mut TableName) {
        unimplemented!()
    }

    fn visit_derived_table(&mut self, table: &mut DerivedTable) {
        self.visit_select(table.subquery.deref_mut());
    }

    fn visit_projects(&mut self, projects: &mut Projects) {
        match projects {
            Projects::NonStar(ref mut columns) => {
                for mut column in columns {
                    self.visit_aliased_column(column);
                }
            }
            Projects::Star => {}
        }
    }

    fn visit_aliased_column(&mut self, column: &mut AliasedColumn) {
        self.visit_expr(&mut column.expr);
    }

    fn visit_scalar_func(&mut self, &mut String, &mut Vec<Expr>) {}

    fn visit_agg_func(&mut self, &mut String, &mut Vec<Expr>) {}

    fn visit_case(&mut self) {}
}
