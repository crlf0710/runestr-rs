use std::{
    cell::{BorrowMutError, RefCell},
    convert::TryFrom,
};

pub(crate) struct RuneRegistry {
    registry_rune_repr_startpos_list: RefCell<Vec<usize>>,
    registry_data_store: RefCell<Vec<u8>>,
}

impl Default for RuneRegistry {
    fn default() -> Self {
        RuneRegistry {
            registry_rune_repr_startpos_list: RefCell::new(vec![0]),
            registry_data_store: RefCell::new(Vec::default()),
        }
    }
}

// FIXME: Remove this when update to Rust 1.55
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ControlFlow<B, C = ()> {
    /// Move on to the next phase of the operation as normal.
    Continue(C),
    /// Exit the operation without running subsequent phases.
    Break(B),
}

impl RuneRegistry {
    pub(crate) fn is_valid_registry_idx(&self, idx: u32) -> bool {
        let last_idx = u32::try_from(self.registry_rune_repr_startpos_list.borrow_mut().len())
            .unwrap()
            .checked_sub(1)
            .unwrap();
        idx <= last_idx
    }
    pub(crate) unsafe fn append_multichar_rune_repr_unchecked(
        &self,
        rune_repr: &[u8],
    ) -> Result<u32, BorrowMutError> {
        let idx;
        if let mut startpos_list = self.registry_rune_repr_startpos_list.try_borrow_mut()? {
            let new_data_store_end;
            if let mut data_store = self.registry_data_store.try_borrow_mut()? {
                data_store.extend_from_slice(rune_repr);
                new_data_store_end = data_store.len();
            } else {
                unreachable!()
            }

            idx = u32::try_from(startpos_list.len() - 1).unwrap();
            startpos_list.push(new_data_store_end);
        } else {
            unreachable!();
        }
        Ok(idx)
    }

    pub(crate) fn iterate_existing_rune_reprs<F, T>(&self, mut f: F) -> Option<T>
    where
        F: FnMut(u32, &[u8]) -> ControlFlow<T>,
    {
        let startpos_list = self.registry_rune_repr_startpos_list.borrow();
        let mut startpos_list = startpos_list.iter().cloned();
        let mut prev_endpos = startpos_list.next().unwrap();
        let startpos_list = startpos_list.enumerate();
        for (idx, endpos) in startpos_list {
            let idx = u32::try_from(idx).unwrap();
            let rune_repr_range = prev_endpos..endpos;
            if let ControlFlow::Break(v) =
                f(idx, &self.registry_data_store.borrow()[rune_repr_range])
            {
                return Some(v);
            }
            prev_endpos = endpos;
        }
        None
    }

    pub(crate) fn with_existing_rune_repr<F, T>(&self, idx: u32, f: F) -> T
    where
        F: FnOnce(&[u8]) -> T,
    {
        let idx = usize::try_from(idx).unwrap();
        let idx_next = idx.checked_add(1).unwrap();

        let rune_repr_range;
        if let startpos_list = self.registry_rune_repr_startpos_list.borrow() {
            rune_repr_range = startpos_list[idx]..startpos_list[idx_next];
        } else {
            unreachable!()
        }

        let result;
        if let data_store = self.registry_data_store.borrow() {
            result = f(&data_store[rune_repr_range]);
        } else {
            unreachable!()
        }
        result
    }
}

thread_local! {
    pub(crate) static THREAD_RUNE_REGISTRY: RuneRegistry = RuneRegistry::default();
}
