use std::fmt;

use proc_macro2::Ident;

pub struct StateTransition<T> {
    pub from_state: usize,
    pub transition: T,
    pub to_state: usize,
}

#[derive(Clone)]
pub struct EndState {
    pub state: usize,
    pub token: Option<Ident>,
}

impl EndState {
    pub fn new(state: usize) -> Self {
        Self { state, token: None }
    }
}

pub struct FiniteAutomaton<S, T> {
    pub num_states: usize,
    pub start_state: usize,
    pub end_states: Vec<EndState>,
    pub transitions: Vec<StateTransition<T>>,
    pub state_info: Vec<S>,
}

impl<S, T: Clone> FiniteAutomaton<S, T> {
    /// Adds the states and transitions of "other" to "self"
    /// They will always be appended after "self" (every state from "other" is offset by num_nodes of "self")
    /// This function creates no connections between them
    pub fn add_automaton(&mut self, other: &Self) {
        for transition in &other.transitions {
            self.transitions.push(StateTransition {
                from_state: transition.from_state + self.num_states,
                transition: transition.transition.clone(),
                to_state: transition.to_state + self.num_states,
            })
        }
        self.num_states += other.num_states;
    }
}

impl<S, T: std::fmt::Debug> fmt::Display for FiniteAutomaton<S, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#={}\ts={}\te={{", self.num_states, self.start_state)?;
        for state in &self.end_states {
            write!(f, "{}({:?}), ", state.state, state.token)?;
        }
        write!(f, "}}")?;

        for transition in &self.transitions {
            write!(
                f,
                "\n{} =={:?}=> {}",
                transition.from_state, transition.transition, transition.to_state
            )?;
        }
        Ok(())
    }
}
