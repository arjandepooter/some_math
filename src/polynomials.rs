use std::{
    fmt::Display,
    ops::{Add, Mul, Neg, Sub},
};

use num_traits::{one, zero, Float, Num, Signed};

#[derive(PartialEq, Debug, Clone)]
pub struct Polynomial<T> {
    coefs: Vec<T>,
}

impl<T> Polynomial<T>
where
    T: Num,
    T: Copy,
{
    pub fn new<I>(coefs: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let coefs: Vec<T> = coefs
            .into_iter()
            .skip_while(|&coef| coef.is_zero())
            .collect();

        Self { coefs }
    }

    pub fn zero() -> Self {
        Self::new(vec![])
    }

    pub fn unit() -> Self {
        Self::new(vec![one()])
    }

    pub fn interpolate<I>(points: I) -> Result<Self, &'static str>
    where
        I: IntoIterator<Item = (T, T)>,
        T: Float,
    {
        let points: Vec<(T, T)> = points.into_iter().collect();

        if points.is_empty() {
            return Err("Needs at least 1 point");
        }

        let result = points.iter().fold(Polynomial::zero(), |sum, &(x, y)| {
            sum + points.iter().filter(|(cx, _)| *cx != x).fold(
                Polynomial::unit(),
                |product, &(cx, _)| {
                    product * Self::new(vec![one::<T>() / (x - cx), -cx / (x - cx)])
                },
            ) * y.into()
        });

        Ok(result)
    }

    pub fn degree(&self) -> isize {
        (self.coefs.len() as isize) - 1
    }

    pub fn evaluate(&self, x: T) -> T {
        let initial = self.coefs.first().map(|&coef| coef).unwrap_or(zero());
        self.coefs
            .iter()
            .skip(1)
            .fold(initial, |result, &coef| result * x + coef)
    }
}

impl<T> Display for Polynomial<T>
where
    T: Signed,
    T: Copy,
    T: Display,
    T: PartialOrd,
{
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "f(x) = ")?;
        let degree = self.degree();

        if degree == -1 {
            return write!(formatter, "0");
        }

        fn format_part<T>(coef: T, degree: isize) -> String
        where
            T: Display + Signed,
        {
            let prefix = if coef == one() {
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
            .filter(|(_, &coef)| coef != zero())
            .fold(start, |result, (offset, &coef)| {
                let op = if coef >= zero() { "+" } else { "-" };

                result.and(write!(
                    formatter,
                    " {} {}",
                    op,
                    format_part(coef, degree - (offset as isize))
                ))
            })
    }
}

impl<T> Add for Polynomial<T>
where
    T: Num,
    T: Copy,
{
    type Output = Polynomial<T>;

    fn add(self, rhs: Self) -> Self::Output {
        if self.degree() < rhs.degree() {
            return rhs + self;
        }

        let rhs_coefs = std::iter::repeat(zero())
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

impl<T> Neg for Polynomial<T>
where
    T: Num,
    T: Copy,
    T: Neg,
    T: Neg<Output = T>,
{
    type Output = Polynomial<T>;

    fn neg(self) -> Self::Output {
        let coefs = self.coefs.into_iter().map(|coef| -coef);

        Self::Output::new(coefs)
    }
}

impl<T> Sub for Polynomial<T>
where
    T: Num,
    T: Copy,
    T: Neg,
    T: Neg<Output = T>,
{
    type Output = Polynomial<T>;

    fn sub(self, rhs: Self) -> Self::Output {
        -rhs + self
    }
}

impl<T> Mul for Polynomial<T>
where
    T: Num,
    T: Copy,
{
    type Output = Polynomial<T>;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.degree() == -1 || rhs.degree() == -1 {
            return Polynomial::zero();
        }

        let mut coefs = vec![zero(); (self.degree() + rhs.degree() + 1) as usize];

        for (offset1, &coef1) in self.coefs.iter().enumerate() {
            for (offset2, &coef2) in rhs.coefs.iter().enumerate() {
                let degree = self.degree() - (offset1 as isize) + rhs.degree() - (offset2 as isize);

                coefs[degree as usize] = coefs[degree as usize] + coef1 * coef2;
            }
        }

        coefs.reverse();

        Self::Output::new(coefs)
    }
}

impl<T> Default for Polynomial<T>
where
    T: Num,
    T: Copy,
{
    fn default() -> Self {
        Polynomial::unit()
    }
}

impl<T> From<T> for Polynomial<T>
where
    T: Num,
    T: Copy,
{
    fn from(n: T) -> Self {
        Self::new(vec![n])
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
        assert_eq!(Polynomial::<f64>::zero().to_string(), "f(x) = 0");
    }

    #[test]
    fn evaluate() {
        assert_eq!(
            Polynomial::new(vec![2.0, -4.0, 0.0, 3.0]).evaluate(3.0),
            21.0
        );
        assert_eq!(Polynomial::zero().evaluate(3.0), 0.0);
    }

    #[test]
    fn interpolate() {
        let points: Vec<(f64, f64)> = vec![(4.0, 4.0), (6.0, 3.0), (8.0, 6.0)];
        assert_eq!(
            Polynomial::interpolate(points),
            Ok(Polynomial::new(vec![0.5, -5.5, 18.0]))
        );

        let points: Vec<(f64, f64)> = Vec::new();
        assert_eq!(
            Polynomial::interpolate(points),
            Err("Needs at least 1 point".into())
        );
    }

    fn integer_polynomial(degree_range: RangeInclusive<usize>) -> BoxedStrategy<Polynomial<i128>> {
        vec(&(-10000i128..10000i128), degree_range)
            .prop_map(Polynomial::new)
            .boxed()
    }

    proptest! {
        #[test]
        fn arbitrary_polynomials(_polynomial in integer_polynomial(0..=10000)) {
        }
    }

    proptest! {
        #[test]
        fn add_zero_identity(p1 in integer_polynomial(0..=1000)) {
            let p2 = p1.clone() + Polynomial::zero();

            assert_eq!(p1, p2);
        }
    }

    proptest! {
        #[test]
        fn add_negate_yields_zero(p1 in integer_polynomial(0..=1000)) {
            let p2 = -p1.clone();

            assert_eq!(p1 + p2, Polynomial::zero());
        }
    }

    proptest! {
        #[test]
        fn add_result_has_same_degree_as_highest_of_inputs(
            p1 in integer_polynomial(0..=100),
            p2 in integer_polynomial(0..=100)
        ) {
            let expected_degree = max(p1.degree(), p2.degree());

            assert_eq!((p1 + p2).degree(), expected_degree);
        }
    }

    proptest! {
        #[test]
        fn addition_associative(
            p1 in integer_polynomial(0..=10),
            p2 in integer_polynomial(0..=10),
            x in 0i128..10i128
        ) {
            let expected = p1.evaluate(x) + p2.evaluate(x);

            assert_eq!((p1 + p2).evaluate(x), expected)
        }
    }

    proptest! {
        #[test]
        fn addition_commutative(
            p1 in integer_polynomial(0..=10),
            p2 in integer_polynomial(0..=10),
            x in 0i128..10i128
        ) {
            let m1 = p1.clone() + p2.clone();
            let m2 = p1 + p2;

            assert_eq!(m1.evaluate(x), m2.evaluate(x))
        }
    }

    proptest! {
        #[test]
        fn multiply_non_zero_polynomials_has_degree_of_sum_of_input_degrees(
            p1 in integer_polynomial(1..=100),
            p2 in integer_polynomial(1..=100)
        ) {
            let expected_degree = p1.degree() + p2.degree();

            assert_eq!((p1 * p2).degree(), expected_degree);
        }
    }

    proptest! {
        #[test]
        fn multiply_unit_identity(
            p in integer_polynomial(1..=100)
        ) {
            let mp = p.clone() * Polynomial::unit();
            assert_eq!(mp, p);
        }
    }

    proptest! {
        #[test]
        fn multiply_with_zero_yields_zero_polynomial(p1 in integer_polynomial(0..=100)) {
            assert_eq!(p1 * Polynomial::zero(), Polynomial::zero());
        }
    }

    proptest! {
        #[test]
        fn multiplication_associative(
            p1 in integer_polynomial(0..=10),
            p2 in integer_polynomial(0..=10),
            x in 0i128..10i128
        ) {
            let expected = p1.evaluate(x) * p2.evaluate(x);

            assert_eq!((p1 * p2).evaluate(x), expected)
        }
    }

    proptest! {
        #[test]
        fn multiplication_commutative(
            p1 in integer_polynomial(0..=10),
            p2 in integer_polynomial(0..=10),
            x in 0i128..10i128
        ) {
            let m1 = p1.clone() * p2.clone();
            let m2 = p1 * p2;

            assert_eq!(m1.evaluate(x), m2.evaluate(x))
        }
    }
}
