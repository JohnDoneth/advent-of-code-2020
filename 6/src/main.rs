use std::iter::FromIterator;

use nom::character::complete;
use nom::multi;
use nom::IResult;

use indexmap::IndexSet;
use nom::sequence;

#[cfg(test)]
use pretty_assertions::assert_eq;

/// Modified macro from the sugars to create an inline IndexSet.
#[macro_export]
macro_rules! iset {
    () => { ::indexmap::set::IndexSet::new() };

    ($($elem: expr),+ $(,)?) => {{
        const CAP: usize = ::sugars::count!($($elem),*);
        let mut set = ::indexmap::set::IndexSet::with_capacity(CAP);
        $(
            let _ = set.insert($elem);
        )+
        set
    }};
}

// Data

#[derive(Debug, PartialEq, Clone)]
struct Groups(Vec<Group>);

impl Groups {
    fn unique_answers_sum(&self) -> usize {
        self.0.iter().map(Group::unique_count).sum()
    }
}

#[test]
fn test_groups_unique_answers_sum() {
    assert_eq!(
        Groups(vec![
            Group(vec![
                Answers(iset! {'a', 'b', 'c', 'x'}),
                Answers(iset! {'a', 'b', 'c', 'x'})
            ]),
            Group(vec![Answers(iset! {'a', 'b', 'c', 'y'})]),
            Group(vec![Answers(iset! {'a', 'b', 'c', 'z'})])
        ])
        .unique_answers_sum(),
        12
    );
}

/// Represents a group of answers.
#[derive(Debug, PartialEq, Clone)]
struct Group(Vec<Answers>);

impl Group {
    fn unique_count(&self) -> usize {
        self.0
            .iter()
            .fold(IndexSet::new(), |mut acc: IndexSet<char>, b| {
                acc.extend(b.0.clone());
                acc
            })
            .len()
    }
}

#[test]
fn test_group_unique_count() {
    assert_eq!(
        Group(vec![Answers(iset! {'a', 'b'}), Answers(iset! {'a', 'c'})]).unique_count(),
        3
    );
}

/// Represents the answers for a single person.
#[derive(Debug, PartialEq, Clone)]
struct Answers(IndexSet<char>);

impl Answers {
    fn is_set(&self, c: char) -> bool {
        self.0.contains(&c)
    }

    fn count(&self) -> usize {
        self.0.len()
    }
}

#[test]
fn test_answers_is_set() {
    assert!(Answers(iset! {'a'}).is_set('a'));
}

#[test]
fn test_answers_count() {
    assert_eq!(Answers(iset! {'a'}).count(), 1);
}

// Parsing

/// Parse Answers
fn parse_answers(i: &str) -> IResult<&str, Answers> {
    let (rest, chars) = multi::many1(complete::one_of("abcdefghijklmnopqrstuvwxyz"))(i)?;
    Ok((rest, Answers(IndexSet::from_iter(chars))))
}

#[test]
fn test_parse_answers() {
    assert_eq!(
        parse_answers("abc"),
        Ok(("", Answers(iset! {'a', 'b', 'c'})))
    )
}

/// Parse Group
fn parse_group(i: &str) -> IResult<&str, Group> {
    let (rest, answers) = multi::many1(sequence::terminated(parse_answers, complete::newline))(i)?;
    Ok((rest, Group(answers)))
}

#[test]
fn test_parse_group() {
    let input = indoc::indoc! {"
        abcx
        abcy
        abcz
    "};

    assert_eq!(
        parse_group(input),
        Ok((
            "",
            Group(vec![
                Answers(iset! {'a', 'b', 'c', 'x'}),
                Answers(iset! {'a', 'b', 'c', 'y'}),
                Answers(iset! {'a', 'b', 'c', 'z'})
            ])
        ))
    )
}

/// Parse Groups
fn parse_groups(i: &str) -> IResult<&str, Groups> {
    let (rest, groups) = multi::many1(sequence::terminated(parse_group, complete::newline))(i)?;
    Ok((rest, Groups(groups)))
}

#[test]
fn test_parse_groups() {
    let input = indoc::indoc! {"
        abcx

        abcy

        abcz

    "};

    assert_eq!(
        parse_groups(input),
        Ok((
            "",
            Groups(vec![
                Group(vec![Answers(iset! {'a', 'b', 'c', 'x'})]),
                Group(vec![Answers(iset! {'a', 'b', 'c', 'y'})]),
                Group(vec![Answers(iset! {'a', 'b', 'c', 'z'})]),
            ]),
        )),
    )
}

#[test]
fn test_parse_sample_answer() {
    let input = indoc::indoc! {"
        abc

        a
        b
        c
        
        ab
        ac
        
        a
        a
        a
        a
        
        b

    "};

    assert_eq!(
        parse_groups(input),
        Ok((
            "",
            Groups(vec![
                Group(vec![Answers(iset! {'a', 'b', 'c'})]),
                Group(vec![
                    Answers(iset! { 'a' }),
                    Answers(iset! { 'b' }),
                    Answers(iset! { 'c' })
                ]),
                Group(vec![
                    Answers(iset! { 'a', 'b' }),
                    Answers(iset! { 'a', 'c' }),
                ]),
                Group(vec![
                    Answers(iset! { 'a' }),
                    Answers(iset! { 'a' }),
                    Answers(iset! { 'a' }),
                    Answers(iset! { 'a' }),
                ]),
                Group(vec![Answers(iset! { 'b' })]),
            ])
        ))
    );
}

#[test]
fn test_run_sample_answer() {
    let input = indoc::indoc! {"
        abc

        a
        b
        c
        
        ab
        ac
        
        a
        a
        a
        a
        
        b

    "};

    let (_, groups) = parse_groups(input).unwrap();

    assert_eq!(groups.unique_answers_sum(), 11);
}

// Execution
fn main() {
    println!("Hello, world!");

    let input = std::fs::read_to_string("input.txt").unwrap();

    let (_, result) = parse_groups(&input).unwrap();

    result
        .0
        .iter()
        .map(|group| (group, group.unique_count()))
        .for_each(|(g, c)| {
            println!("{:#?}", (g, c));
        });

    println!("{:?}", result.unique_answers_sum());
}
