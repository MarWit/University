#![feature(try_blocks)]
use std::collections::{hash_map::Entry, BTreeMap, HashMap};
use std::io::{Read, Write};

use bitvec::vec::BitVec;
use error::SequiturError;
use generational_arena::{Arena, Index};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use slotmap::{DefaultKey, Key, SlotMap};

pub mod error;
#[cfg(test)]
mod tests;

pub type Result<T> = std::result::Result<T, SequiturError>;

macro_rules! zip {
    ($($e:expr),+) => {try { ($($e?),+) }}
}

#[derive(Hash, PartialEq, Eq, Copy, Clone)]
enum Symbol<T> {
    Terminal(T),
    Nonterminal(DefaultKey),
    Guard(DefaultKey),
}

type Digram<T> = (Symbol<T>, Symbol<T>);

#[derive(Hash, Eq, Copy, Clone, PartialEq)]
struct Node<T> {
    symbol: Symbol<T>,
    next: Option<Index>,
    prev: Option<Index>,
}

impl<T> Symbol<T> {
    fn is_guard(&self) -> bool {
        match self {
            Symbol::Guard(_) => true,
            _ => false,
        }
    }
}

#[derive(Copy, Clone)]
struct Production {
    guard: Index,
    frequency: usize,
}

struct ProductionIterator<'a, T>
where
    T: std::hash::Hash + Copy + Eq,
{
    sequitur: &'a Sequitur<T>,
    current: Option<Index>,
}

#[derive(Serialize, Deserialize)]
enum EncodedSymbol<T> {
    Marker,
    Terminal(T),
    Nonterminal(u16),
    Guard(u16, u16),
}

#[derive(Default)]
pub struct Sequitur<T: Eq + std::hash::Hash + Copy> {
    start: DefaultKey,
    arena: Arena<Node<T>>,
    productions: SlotMap<DefaultKey, Production>,
    digrams: HashMap<Digram<T>, Index>,
}

impl<T> Node<T>
where
    T: std::hash::Hash + Copy + Eq,
{
    pub fn guard(arena: &mut Arena<Node<T>>, production: DefaultKey) -> Index {
        let index = arena.insert(Self {
            symbol: Symbol::Guard(production),
            next: None,
            prev: None,
        });

        arena[index].prev = Some(index);
        arena[index].next = Some(index);

        index
    }

    pub fn create(sequitur: &mut Sequitur<T>, symbol: Symbol<T>) -> Index {
        let index = sequitur.arena.insert(Self {
            symbol,
            next: None,
            prev: None,
        });

        if let Symbol::Nonterminal(production) = symbol {
            Production::reuse(sequitur, production);
        }

        if symbol.is_guard() {
            sequitur.arena[index].prev = Some(index);
            sequitur.arena[index].next = Some(index);
        }

        index
    }

    pub fn insert_after(sequitur: &mut Sequitur<T>, this: Index, what: Index) {
        if let Some(next) = sequitur.arena[this].next {
            Node::connect(sequitur, what, next);
        }
        Node::connect(sequitur, this, what);
    }

    fn connect(sequitur: &mut Sequitur<T>, left: Index, right: Index) {
        let left_node = sequitur.arena[left];

        if let Some(left_next_node) = left_node.next.map(|index| sequitur.arena[index]) {
            if !left_next_node.symbol.is_guard() {
                Node::delete_digram(sequitur, left);

                // NOTE(MarWit): This is pretty much the same code. Refactor?
                let right_node = sequitur.arena[right];
                if let Some((right_prev, right_next)) = zip!(right_node.prev, right_node.next) {
                    let right_prev_node = sequitur.arena[right_prev];
                    let right_next_node = sequitur.arena[right_next];

                    if right_prev_node.symbol == right_node.symbol
                        && right_next_node.symbol == right_node.symbol
                    {
                        let digram = (right_node.symbol, right_next_node.symbol);
                        sequitur.digrams.insert(digram, right);
                    }
                }

                let left_node = sequitur.arena[left];
                if let Some((left_prev, left_next)) = zip!(left_node.prev, left_node.next) {
                    let left_prev_node = sequitur.arena[left_prev];
                    let left_next_node = sequitur.arena[left_next];

                    if left_prev_node.symbol == left_node.symbol
                        && left_next_node.symbol == left_node.symbol
                    {
                        let digram = (left_prev_node.symbol, left_node.symbol);
                        sequitur.digrams.insert(digram, left_prev);
                    }
                }
            }
        }

        let arena = &mut sequitur.arena;
        arena[left].next = Some(right);
        arena[right].prev = Some(left);
    }

    fn check(sequitur: &mut Sequitur<T>, this: Index) -> bool {
        let node = &sequitur.arena[this];

        if node.symbol.is_guard() {
            return false;
        }

        let next = if let Some(next) = node.next {
            &sequitur.arena[next]
        } else {
            return false;
        };

        if next.symbol.is_guard() {
            return false;
        }

        let digram = (node.symbol, next.symbol);
        let entry = match sequitur.digrams.entry(digram) {
            Entry::Occupied(entry) => entry,
            Entry::Vacant(entry) => {
                entry.insert(this);
                return false;
            }
        };

        let found_index = *entry.get();
        let found = sequitur.arena[found_index];
        if found.next.map(|index| index != this).unwrap_or(false) {
            Node::matching(sequitur, this, found_index);
        }

        true
    }

    fn matching(sequitur: &mut Sequitur<T>, this: Index, with: Index) {
        let with_node = sequitur.arena[with];
        let with_next_node = sequitur.arena[with_node.next.unwrap()]; // NOTE(MarWit): This is safe
        let with_node_prev = sequitur.arena[with_node.prev.unwrap()]; // XXX(MarWit): This is _probably_ safe

        let production_index;

        if with_node_prev.symbol.is_guard()
            && with_next_node
                .next
                .map(|index| sequitur.arena[index].symbol.is_guard())
                .unwrap_or(false)
        {
            if let Symbol::Guard(production) = with_node_prev.symbol {
                production_index = production;
                Node::substitute(sequitur, this, production)
            } else {
                return;
            }
        } else {
            production_index = Production::empty(sequitur);

            let node = sequitur.arena[this];
            let next_node = sequitur.arena[node.next.unwrap()]; // NOTE(MarWit): This is safe
            let production = sequitur.productions[production_index];

            let first_node = Node::create(sequitur, node.symbol);
            Node::insert_after(sequitur, production.last(&sequitur.arena), first_node);

            let second_node = Node::create(sequitur, next_node.symbol);
            Node::insert_after(sequitur, production.last(&sequitur.arena), second_node);

            Node::substitute(sequitur, with, production_index);
            Node::substitute(sequitur, this, production_index);

            let digram = (node.symbol, next_node.symbol);
            sequitur.digrams.insert(digram, first_node);
        }

        let production = sequitur.productions[production_index];
        let first = production.first(&sequitur.arena);
        let first_node = sequitur.arena[first];

        if let Symbol::Nonterminal(first_production_index) = first_node.symbol {
            let first_production = sequitur.productions[first_production_index];
            if first_production.frequency == 1 {
                Node::expand(sequitur, first);
            }
        }
    }

    fn expand(sequitur: &mut Sequitur<T>, this: Index) {
        let node = sequitur.arena[this];
        if let Symbol::Nonterminal(old_production) = node.symbol {
            // NOTE(MarWit): This `if let` will happen every time
            let prev = node.prev.unwrap(); // NOTE(MarWit): This is safe, as it is digram
            let next = node.next.unwrap(); // NOTE(MarWit): This is safe, as it is digram
            let (first, last) = {
                let old_production = sequitur.productions[old_production];
                (
                    old_production.first(&sequitur.arena),
                    old_production.last(&sequitur.arena),
                )
            };

            Node::remove(sequitur, sequitur.productions[old_production].guard);
            sequitur.productions.remove(old_production);
            let next_node = sequitur.arena[next];
            let last_node = sequitur.arena[last];
            let digram = (node.symbol, next_node.symbol);
            let digram_index = sequitur.digrams[&digram];

            if digram_index == this {
                sequitur.digrams.remove(&digram);
            }

            Node::remove(sequitur, this);
            Node::connect(sequitur, prev, first);
            Node::connect(sequitur, last, next);
            sequitur
                .digrams
                .insert((last_node.symbol, next_node.symbol), last);
        }
    }

    fn substitute(sequitur: &mut Sequitur<T>, this: Index, rule: DefaultKey) {
        let prev_index = sequitur.arena[this].prev;

        Node::remove(sequitur, this);

        if let Some(prev_index) = prev_index {
            if let Some(prev_next_index) = sequitur.arena[prev_index].next {
                Node::remove(sequitur, prev_next_index);
            }

            let new_node = Node::create(sequitur, Symbol::Nonterminal(rule));
            Node::insert_after(sequitur, prev_index, new_node);

            if !Node::check(sequitur, prev_index) {
                Node::check(sequitur, new_node);
            }
        }
    }

    fn remove(sequitur: &mut Sequitur<T>, this: Index) {
        let node = sequitur.arena[this];

        if let Some((prev, next)) = zip!(node.prev, node.next) {
            Node::connect(sequitur, prev, next);
        }

        if !node.symbol.is_guard() {
            Node::delete_digram(sequitur, this);
        }

        if let Symbol::Nonterminal(production) = node.symbol {
            Production::deuse(sequitur, production);
        }

        sequitur.arena.remove(this);
    }

    fn delete_digram(sequitur: &mut Sequitur<T>, this: Index) {
        let node = &sequitur.arena[this];
        let next = if let Some(next) = node.next {
            &sequitur.arena[next]
        } else {
            return;
        };

        if node.symbol.is_guard() || next.symbol.is_guard() {
            return;
        }

        let digram = (node.symbol, next.symbol);
        if !sequitur.digrams.contains_key(&digram) {
            return;
        }

        let digram_index = sequitur.digrams[&digram];
        if digram_index == this {
            sequitur.digrams.remove(&digram);
        }
    }
}

impl Production {
    pub fn empty<T>(sequitur: &mut Sequitur<T>) -> DefaultKey
    where
        T: std::hash::Hash + Eq + Copy,
    {
        let guard = Node::guard(&mut sequitur.arena, slotmap::DefaultKey::null());
        let production_index = sequitur.productions.insert(Self {
            guard,
            frequency: 0,
        });
        sequitur.arena[guard].symbol = Symbol::Guard(production_index);
        production_index
    }

    pub fn put<T>(&self, sequitur: &mut Sequitur<T>, symbol: Symbol<T>)
    where
        T: std::hash::Hash + Eq + Copy,
    {
        let last = self.last(&sequitur.arena);
        let node = Node::create(sequitur, symbol);
        Node::insert_after(sequitur, last, node);
        Node::check(sequitur, last);
    }

    pub fn put_unchecked<T>(&self, sequitur: &mut Sequitur<T>, symbol: Symbol<T>)
    where
        T: std::hash::Hash + Eq + Copy,
    {
        let last = self.last(&sequitur.arena);
        let node = Node::create(sequitur, symbol);
        Node::insert_after(sequitur, last, node);
    }

    pub fn first<T>(&self, arena: &Arena<Node<T>>) -> Index {
        arena[self.guard].next.unwrap() // NOTE(MarWit): This is safe
    }

    pub fn last<T>(&self, arena: &Arena<Node<T>>) -> Index {
        arena[self.guard].prev.unwrap() // NOTE(MarWit): This is safe
    }

    pub fn iter<'s, T>(&self, sequitur: &'s Sequitur<T>) -> ProductionIterator<'s, T>
    where
        T: std::hash::Hash + Eq + Copy,
    {
        ProductionIterator {
            sequitur,
            current: Some(self.first(&sequitur.arena)),
        }
    }

    #[allow(dead_code)]
    pub fn repr<T>(&self, sequitur: &Sequitur<T>) -> String
    where
        T: std::hash::Hash + Eq + Copy + std::fmt::Display,
    {
        let mut buffer = String::new();
        for e in self.iter(sequitur) {
            match e {
                Symbol::Guard(_) => break,
                Symbol::Nonterminal(idx) => {
                    buffer.push_str(&format!("{:?}", idx));
                }
                Symbol::Terminal(c) => buffer.push_str(&format!("{}", c)),
            }
        }

        buffer
    }

    pub fn deuse<T>(sequitur: &mut Sequitur<T>, this: DefaultKey)
    where
        T: std::hash::Hash + Eq + Copy,
    {
        if sequitur.productions.contains_key(this) {
            sequitur.productions[this].frequency -= 1;
        }
    }

    pub fn reuse<T>(sequitur: &mut Sequitur<T>, this: DefaultKey)
    where
        T: std::hash::Hash + Eq + Copy,
    {
        if sequitur.productions.contains_key(this) {
            sequitur.productions[this].frequency += 1;
        }
    }
}

impl<T> Iterator for ProductionIterator<'_, T>
where
    T: Eq + Copy + std::hash::Hash,
{
    type Item = Symbol<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.sequitur.arena[self.current?];
        if node.symbol.is_guard() {
            None
        } else {
            let symbol = node.symbol;
            self.current = node.next;
            Some(symbol)
        }
    }
}

impl<T> Sequitur<T>
where
    T: Eq + Copy + std::hash::Hash,
{
    pub fn new() -> Self {
        let mut sequitur = Self {
            start: DefaultKey::default(),
            arena: Arena::new(),
            productions: SlotMap::default(),
            digrams: Default::default(),
        };
        let start = Production::empty(&mut sequitur);
        sequitur.start = start;
        sequitur
    }

    pub fn put(&mut self, value: T) {
        let start = self.productions[self.start];
        start.put(self, Symbol::Terminal(value));
    }

    fn encode(&self) -> Vec<EncodedSymbol<T>> {
        let mut output = vec![];
        let mut stack = vec![];
        let mut index = Some(self.productions[self.start].first(&self.arena));
        let mut mapping = HashMap::<_, (u16, usize, u16)>::new();
        let mut position = 0;
        let mut productions_index = 1;

        while let Some(idx) = index {
            let node = self.arena[idx];
            match node.symbol {
                Symbol::Terminal(c) => {
                    output.push(EncodedSymbol::Terminal(c));
                    index = node.next;
                }
                Symbol::Nonterminal(production) => match mapping.entry(production) {
                    Entry::Occupied(mut entry) => {
                        let (i, ..) = entry.get_mut();
                        output.push(EncodedSymbol::Nonterminal(*i));
                        index = node.next;
                    }
                    Entry::Vacant(entry) => {
                        let len = self.productions[production].iter(self).count();
                        entry.insert((productions_index, position, len as _));
                        output.push(EncodedSymbol::Marker);
                        productions_index += 1;
                        stack.push(node.next);
                        index = Some(self.productions[production].first(&self.arena));
                        continue;
                    }
                },
                Symbol::Guard(production) => {
                    if production != self.start {
                        let (i, _, len) = mapping[&production];
                        output.push(EncodedSymbol::Guard(i, len));
                    }
                    index = stack.pop().flatten();
                }
            }

            position += 1;
        }

        output
    }

    fn decode(encoded: Vec<EncodedSymbol<T>>) -> Result<Self> {
        let mut sequitur = Self::new();
        let mut mapping = vec![sequitur.start];
        let mut buffer = vec![];

        for symbol in encoded {
            match symbol {
                EncodedSymbol::Guard(idx, len) => {
                    let production_index = mapping[idx as usize];
                    let offset = buffer
                        .iter()
                        .position(|&e| e == Symbol::Guard(production_index))
                        .ok_or(SequiturError::InvalidEncodedTokenStreamError)?;
                    for symbol in buffer
                        .splice(offset..=offset + len as usize, vec![Symbol::Nonterminal(
                            production_index,
                        )])
                        .skip(1)
                    {
                        let production = sequitur.productions[production_index];
                        production.put_unchecked(&mut sequitur, symbol);
                    }
                }
                EncodedSymbol::Nonterminal(idx) => {
                    buffer.push(Symbol::Nonterminal(mapping[idx as usize]));
                }
                EncodedSymbol::Terminal(t) => {
                    buffer.push(Symbol::Terminal(t));
                }
                EncodedSymbol::Marker => {
                    let production = Production::empty(&mut sequitur);
                    mapping.push(production);
                    buffer.push(Symbol::Guard(production));
                }
            }
        }

        for symbol in buffer {
            let production = sequitur.productions[sequitur.start];
            production.put_unchecked(&mut sequitur, symbol);
        }

        Ok(sequitur)
    }

    fn build_production(&self, production_index: DefaultKey) -> Vec<T> {
        let production = self.productions[production_index];
        let mut output = vec![];

        for symbol in production.iter(self) {
            match symbol {
                Symbol::Terminal(c) => {
                    output.push(c);
                }
                Symbol::Nonterminal(this_production) => {
                    output.extend(self.build_production(this_production).into_iter());
                }
                Symbol::Guard(_) => {
                    break;
                }
            }
        }

        output
    }

    pub fn rebuild(&self) -> Vec<T> {
        self.build_production(self.start)
    }
}

impl<T> Sequitur<T>
where
    T: Eq + Copy + Serialize + DeserializeOwned + std::hash::Hash,
{
    pub fn compress<W: Write>(&self, into: &mut W) -> Result<()> {
        let encoded = bincode::serialize(&self.encode())?;
        let mut weights = HashMap::new();

        for &e in &encoded {
            *weights.entry(e).or_insert(0) += 1;
        }

        let (_, tree) = huffman_compress::codebook(&weights);
        let canonical_codebook = tree.canonical();

        let mut buffer = BitVec::default();
        for e in &encoded {
            let _ = canonical_codebook.encode(&mut buffer, e);
        }

        let mut mapping = vec![];
        for i in 0x0..=0xff {
            if let Some(bits) = canonical_codebook.get(&i) {
                assert!(bits.len() < 0xff);
                mapping.push(bits.len() as u8 + 1);
            } else {
                mapping.push(0);
            }
        }

        bincode::serialize_into(into, &(buffer.into_vec(), mapping, encoded.len()))?;
        Ok(())
    }

    pub fn decompress<R: Read>(from: &mut R) -> Result<Sequitur<T>> {
        let (data, encoded_tree, length): (Vec<usize>, Vec<u8>, usize) =
            bincode::deserialize_from(from)?;

        let buffer = BitVec::<bitvec::order::Lsb0, usize>::from_vec(data);
        let mut mapping = BTreeMap::new();

        for (i, l) in encoded_tree
            .into_iter()
            .enumerate()
            .filter(|(_, l)| *l != 0)
            .map(|(i, l)| (i, l - 1))
        {
            mapping
                .entry(l as usize)
                .or_insert_with(Vec::default)
                .push(i as u8);
        }

        let codebook = huffman_compress::Book::from_lengths(mapping);
        let tree = huffman_compress::Tree::from_codebook(&codebook);

        let serialized = tree
            .decoder(buffer.into_iter(), length)
            .collect::<Vec<u8>>();
        let encoded: Vec<EncodedSymbol<T>> = bincode::deserialize_from(&serialized[..])?;
        Sequitur::decode(encoded)
    }
}
