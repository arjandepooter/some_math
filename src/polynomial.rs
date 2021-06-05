use std::{
    fmt::Display,
    ops::{Add, Mul, Neg, Sub},
};

#[derive(PartialEq, Debug, Clone)]
pub struct Polynomial {
    coefs: Vec<f64>,
}

impl Polynomial {
    pub fn new<T>(coefs: T) -> Self
    where
        T: IntoIterator<Item = f64>,
    {
        let coefs: Vec<f64> = coefs
            .into_iter()
            .map(|coef| coef)
            .skip_while(|&coef| coef == 0f64)
            .collect();

        Self { coefs }
    }

    pub fn zero() -> Self {
        Self::new(vec![])
    }

    pub fn unit() -> Self {
        Self::new(vec![1.0])
    }

    pub fn degree(&self) -> isize {
        (self.coefs.len() as isize) - 1
    }

    pub fn evaluate(&self, x: f64) -> f64 {
        let initial = self.coefs.first().map(|&coef| coef).unwrap_or_default();
        self.coefs
            .iter()
            .skip(1)
            .fold(initial, |result, &coef| result * x + coef)
    }
}

impl Display for Polynomial {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "f(x) = ")?;
        let degree = self.degree();

        if degree == -1 {
            return write!(formatter, "0");
        }

        fn format_part(coef: f64, degree: isize) -> String {
            let prefix = if coef == 1.0 {
                "".to_string()
            } else {
                format!("{}", coef.abs())
            };

            let var = match degree {
                0 => "".to_string(),
                1 => "x".to_string(),
                _ => format!("x^{}", degree),
            };

            format!("{}{}", prefix, var)
        }

        let start = write!(
            formatter,
            "{}",
            format_part(*self.coefs.first().unwrap(), degree)
        );
        self.coefs
            .iter()
            .enumerate()
            .skip(1)
            .filter(|(_, &coef)| coef != 0.0)
            .fold(start, |result, (offset, &coef)| {
                let op = if coef >= 0.0 { "+" } else { "-" };

                result.and(write!(
                    formatter,
                    " {} {}",
                    op,
                    format_part(coef, degree - (offset as isize))
                ))
            })
    }
}

impl Add for Polynomial {
    type Output = Polynomial;

    fn add(self, rhs: Self) -> Self::Output {
        if self.degree() < rhs.degree() {
            return rhs + self;
        }

        let rhs_coefs = std::iter::repeat(0f64)
            .take((self.degree() - rhs.degree()) as usize)
            .chain(rhs.coefs);

        let coefs = self
            .coefs
            .into_iter()
            .zip(rhs_coefs)
            .map(|(c0, c1)| c0 + c1);

        Polynomial::new(coefs)
    }
}

impl Neg for Polynomial {
    type Output = Polynomial;

    fn neg(self) -> Self::Output {
        let coefs = self.coefs.into_iter().map(|coef| -coef);

        Self::Output::new(coefs)
    }
}

impl Sub for Polynomial {
    type Output = Polynomial;

    fn sub(self, rhs: Self) -> Self::Output {
        -rhs + self
    }
}

impl Mul for Polynomial {
    type Output = Polynomial;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.degree() == -1 || rhs.degree() == -1 {
            return Polynomial::zero();
        }

        let mut coefs = vec![0.0; (self.degree() + rhs.degree() + 1) as usize];

        for (offset1, &coef1) in self.coefs.iter().enumerate() {
            for (offset2, &coef2) in rhs.coefs.iter().enumerate() {
                let degree = self.degree() - (offset1 as isize) + rhs.degree() - (offset2 as isize);

                coefs[degree as usize] += coef1 * coef2;
            }
        }

        coefs.reverse();

        Self::Output::new(coefs)
    }
}

impl Default for Polynomial {
    fn default() -> Self {
        Polynomial::unit()
    }
}

#[cfg(test)]
mod tests {
    use std::{cmp::max, ops::RangeInclusive, vec};

    use super::*;
    use proptest::{collection::vec, prelude::*, strategy::Strategy};

    #[test]
    fn trailing_zero_coefs() {
        let p = Polynomial::new(vec![0f64, 0f64, 1f64, 0f64, 3f64]);

        assert_eq!(p, Polynomial::new(vec![1f64, 0f64, 3f64]));
    }

    #[test]
    fn add_polynomials() {
        let p1 = Polynomial::new(vec![1.0, 2.0, 3.0, 0.0]);
        let p2 = Polynomial::new(vec![1.0, 2.0, 3.0]);
        let expected = Polynomial::new(vec![1.0, 3.0, 5.0, 3.0]);

        assert_eq!(p1 + p2, expected);
    }

    #[test]
    fn multiply_polynomials() {
        let p1 = Polynomial::new(vec![1.0, 3.0, 0.0]);
        let p2 = Polynomial::new(vec![2.0, 3.0]);
        let expected = Polynomial::new(vec![2.0, 9.0, 9.0, 0.0]);

        assert_eq!(p1 * p2, expected);
    }

    #[test]
    fn display_polynomials() {
        assert_eq!(
            Polynomial::new(vec![1.0, 0.0, -3.0, 1.5]).to_string(),
            "f(x) = x^3 - 3x + 1.5"
        );
        assert_eq!(Polynomial::zero().to_string(), "f(x) = 0");
    }

    #[test]
    fn evaluate() {
        assert_eq!(
            Polynomial::new(vec![2.0, -4.0, 0.0, 3.0]).evaluate(3.0),
            21.0
        );
        assert_eq!(Polynomial::zero().evaluate(3.0), 0.0);
    }

    fn arbitrary_polynomial(degree_range: RangeInclusive<usize>) -> BoxedStrategy<Polynomial> {
        vec(&(-10000f64..10000f64), degree_range)
            .prop_map(Polynomial::new)
            .boxed()
    }

    proptest! {
        #[test]
        fn arbitrary_polynomials(_polynomial in arbitrary_polynomial(0..=10000)) {
        }
    }

    proptest! {
        #[test]
        fn add_zero_identity(p1 in arbitrary_polynomial(0..=1000)) {
            let p2 = p1.clone() + Polynomial::zero();

            assert_eq!(p1, p2);
        }
    }

    proptest! {
        #[test]
        fn add_negate_yields_zero(p1 in arbitrary_polynomial(0..=1000)) {
            let p2 = -p1.clone();

            assert_eq!(p1 + p2, Polynomial::zero());
        }
    }

    proptest! {
        #[test]
        fn add_result_has_same_degree_as_highest_of_inputs(
            p1 in arbitrary_polynomial(0..=100),
            p2 in arbitrary_polynomial(0..=100)
        ) {
            let expected_degree = max(p1.degree(), p2.degree());

            assert_eq!((p1 + p2).degree(), expected_degree);
        }
    }

    proptest! {
        #[test]
        fn multiply_non_zero_polynomials_has_degree_of_sum_of_input_degrees(
            p1 in arbitrary_polynomial(1..=100),
            p2 in arbitrary_polynomial(1..=100)
        ) {
            let expected_degree = p1.degree() + p2.degree();

            assert_eq!((p1 * p2).degree(), expected_degree);
        }
    }

    proptest! {
        #[test]
        fn multiply_unit_identity(
            p in arbitrary_polynomial(1..=100)
        ) {
            let mp = p.clone() * Polynomial::unit();
            assert_eq!(mp, p);
        }
    }

    proptest! {
        #[test]
        fn multiply_with_zero_yields_zero_polynomial(p1 in arbitrary_polynomial(0..=100)) {
            assert_eq!(p1 * Polynomial::zero(), Polynomial::zero());
        }
    }
}
