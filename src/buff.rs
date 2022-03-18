use std::fmt::Display;

/// Buffer data structure for parsing
pub struct Buff<T> {
    data: Vec<T>,
    pos: usize,
    stack: Vec<usize>,
}

impl<T> Buff<T>
where
    T: Display,
    T: Eq,
    T: Clone,
{
    /// Create a new buffer
    pub fn new(data: Vec<T>) -> Self {
        Buff {
            data,
            pos: 0,
            stack: vec![0],
        }
    }

    /// Push the current position to the stack
    pub fn save(&mut self) {
        self.stack.push(self.pos);
    }

    /// Pop the last position pushed to the stack
    /// and set the current position to it
    pub fn restore(&mut self) {
        self.pos = self.stack.pop().unwrap();
    }

    /// Update the top of the stack
    /// to be the current position
    pub fn resave(&mut self) {
        self.stack.pop();
        self.stack.push(self.pos);
    }

    /// Get the first element of the buffer
    /// and returns `None` if the buffer is empty
    pub fn top(&self) -> Option<T> {
        self.data.get(self.pos).cloned()
    }

    /// Check if there are still elements to read
    /// in the buffer
    pub fn is_empty(&self) -> bool {
        self.pos >= self.data.len()
    }

    /// Drop the first element of the buffer
    /// and returns `None` if the buffer is empty
    pub fn pop(&mut self) -> Option<()> {
        if self.is_empty() {
            None
        } else {
            self.pos += 1;
            Some(())
        }
    }

    /// Get the first element of the buffer
    /// and drops it.
    /// Returns `None` if the buffer is empty
    pub fn next(&mut self) -> Option<T> {
        let x = self.top()?;
        self.pop()?;
        Some(x)
    }

    /// Compare a given element with the first
    /// element of the buffer and drops it if they
    /// are equals.
    /// Returns `None` if the buffer is empty
    pub fn expect(&mut self, x: T) -> Option<()> {
        let y = self.next()?;
        if x == y {
            Some(())
        } else {
            None
        }
    }

    /// Expect the current element of the buffer to
    /// satisfies a condition and returns it.
    pub fn expect_cond<P>(&mut self, cond: P) -> Option<T>
    where
        P: FnOnce(&T) -> bool,
    {
        let y = self.next()?;
        if cond(&y) {
            Some(y)
        } else {
            None
        }
    }

    /// Expect the current element of the buffer to
    /// be convertible.
    /// The conversion function is expected to return `None` if
    /// the element is not convertible
    pub fn expect_convert<U>(&mut self, conv: fn(T) -> Option<U>) -> Option<U> {
        conv(self.next()?)
    }

    /// Expect a non-empty sequence of
    /// elements satisfying a given condition.
    /// Elements are converted according to a given conversion
    /// function.
    pub fn convert_while<U>(&mut self, pre: fn(&T) -> bool, conv: fn(T) -> U) -> Option<Vec<U>> {
        let mut list = vec![conv(self.expect_cond(pre)?)];
        self.save();
        while let Some(l) = self.expect_cond(pre) {
            list.push(conv(l));
            self.resave();
        }
        self.restore();
        Some(list)
    }

    /// Expect a non-empty sequence of
    /// elements satisfying a given condition.
    /// The condition is given as a conversion function
    /// returning `None` if the element violates the condition
    pub fn convert_list<U>(&mut self, conv: fn(T) -> Option<U>) -> Option<Vec<U>> {
        let mut list = vec![conv(self.next()?)?];
        self.save();
        while let Some(l) = self.expect_convert(conv) {
            list.push(l);
            self.resave();
        }
        self.restore();
        Some(list)
    }

    /// Parse a non-empty list of elements according
    /// to a parsing function
    pub fn expect_list<U>(&mut self, parse: fn(&mut Buff<T>) -> Option<U>) -> Option<Vec<U>> {
        let mut list = vec![parse(self)?];
        self.save();
        while let Some(l) = parse(self) {
            list.push(l);
            self.resave();
        }
        self.restore();
        Some(list)
    }

    pub fn expect_end(&self) -> Option<()> {
        if self.top().is_some() {
            None
        } else {
            Some(())
        }
    }

    pub fn expect_one_of(&mut self, alt: Vec<T>) -> Option<()> {
        let y = self.next()?;
        if alt.contains(&y) {
            Some(())
        } else {
            None
        }
    }

    pub fn check<P>(&self, pred: P) -> bool
    where
        P: FnOnce(T) -> bool,
    {
        if let Some(c) = self.top() {
            pred(c)
        } else {
            false
        }
    }
}

impl Buff<char> {
    fn is_space(&self) -> bool {
        self.check(|c| vec!['\t', '\n', ' '].contains(&c))
    }

    pub fn trim(&mut self) {
        while !self.is_empty() && self.is_space() {
            self.pos += 1;
        }
    }

    pub fn expect_u32(&mut self) -> Option<u32> {
        self.trim();
        let mut num = self.expect_digit()?.to_digit(10).unwrap();
        while let Some(c) = self.top() {
            if c.is_digit(10) {
                self.pop();
                num += 10 * num + c.to_digit(10).unwrap();
            } else {
                break;
            }
        }
        Some(num)
    }

    pub fn expect_alpha(&mut self) -> Option<char> {
        let c = self.next()?;
        if c.is_ascii_alphabetic() {
            Some(c)
        } else {
            None
        }
    }

    pub fn expect_digit(&mut self) -> Option<char> {
        let c = self.next()?;
        if c.is_digit(10) {
            Some(c)
        } else {
            None
        }
    }

    pub fn expect_symb(&mut self) -> Option<String> {
        self.trim();
        let mut symb = String::new();
        symb.push(self.expect_alpha()?);
        while let Some(c) = self.top() {
            if c.is_alphanumeric() {
                self.pop();
                symb.push(c);
            } else {
                break;
            }
        }
        Some(symb)
    }

    pub fn expect_blank(&mut self) -> Option<()> {
        self.expect_one_of(vec![' ', '\t', '\n'])
    }

    pub fn expect_token(&mut self, tok: String) -> Option<()> {
        let symb = self.expect_symb()?;
        if symb == tok {
            Some(())
        } else {
            None
        }
    }
}
