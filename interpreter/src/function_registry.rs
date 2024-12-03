use crate::macros::impl_handler;
use crate::{FunctionContext, ResolveResult};
use cel_parser::*;
use std::collections::HashMap;
use std::marker::PhantomData;

pub struct WithFunctionContext;

impl_handler!();
impl_handler!(C1);
impl_handler!(C1, C2);
impl_handler!(C1, C2, C3);
impl_handler!(C1, C2, C3, C4);
impl_handler!(C1, C2, C3, C4, C5);
impl_handler!(C1, C2, C3, C4, C5, C6);
impl_handler!(C1, C2, C3, C4, C5, C6, C7);
impl_handler!(C1, C2, C3, C4, C5, C6, C7, C8);
impl_handler!(C1, C2, C3, C4, C5, C6, C7, C8, C9);

// Heavily inspired by https://users.rust-lang.org/t/common-data-type-for-functions-with-different-parameters-e-g-axum-route-handlers/90207/6
// and https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=c6744c27c2358ec1d1196033a0ec11e4

#[derive(Default)]
pub struct FunctionRegistry<Key>
where
    Key: Dispatchable,
{
    functions: HashMap<Key, Box<dyn Function>>,
}

impl<Key> FunctionRegistry<Key>
where
    Key: Dispatchable,
{
    pub(crate) fn add<H, T>(&mut self, name: &Key, handler: H)
    where
        H: Handler<T> + 'static + Send + Sync,
        T: 'static,
    {
        self.functions.insert(
            name.clone(),
            Box::new(HandlerFunction {
                handler,
                into_callable: |h, ctx| Box::new(HandlerCallable::new(h, ctx)),
            }),
        );
    }

    pub(crate) fn get(&self, name: &Key) -> Option<Box<dyn Function>> {
        self.functions.get(name).map(|f| f.clone_box())
    }

    pub(crate) fn has(&self, name: &Key) -> bool {
        self.functions.contains_key(name)
    }
}

/// A trait for types that can be converted into a [`ResolveResult`] by calling
/// as a function with a [`FunctionContext`] to resolve the values of its arguments.
pub trait Function: Send + Sync {
    fn clone_box(&self) -> Box<dyn Function>;
    fn into_callable<'a>(self: Box<Self>, ctx: &'a mut FunctionContext) -> Box<dyn Callable + 'a>;
    fn call_with_context(self: Box<Self>, ctx: &mut FunctionContext) -> ResolveResult;
}

pub struct HandlerFunction<H: Clone + Send + Sync> {
    pub handler: H,
    pub into_callable: for<'a> fn(H, &'a mut FunctionContext) -> Box<dyn Callable + 'a>,
}

impl<H: Clone + Send + Sync> Clone for HandlerFunction<H> {
    fn clone(&self) -> Self {
        Self {
            handler: self.handler.clone(),
            into_callable: self.into_callable,
        }
    }
}

impl<H> Function for HandlerFunction<H>
where
    H: Clone + Send + Sync + 'static,
{
    fn clone_box(&self) -> Box<dyn Function> {
        Box::new(self.clone())
    }

    fn into_callable<'a>(self: Box<Self>, ctx: &'a mut FunctionContext) -> Box<dyn Callable + 'a> {
        (self.into_callable)(self.handler, ctx)
    }

    fn call_with_context(self: Box<Self>, ctx: &mut FunctionContext) -> ResolveResult {
        self.into_callable(ctx).call()
    }
}

pub trait Callable {
    fn call(&mut self) -> ResolveResult;
}

pub struct HandlerCallable<'a, 'context, H, T> {
    handler: H,
    context: &'a mut FunctionContext<'context>,
    _marker: PhantomData<fn() -> T>,
}

impl<'a, 'context, H, T> HandlerCallable<'a, 'context, H, T> {
    pub fn new(handler: H, ctx: &'a mut FunctionContext<'context>) -> Self {
        Self {
            handler,
            context: ctx,
            _marker: PhantomData,
        }
    }
}

impl<'a, 'context, H, T> Callable for HandlerCallable<'a, 'context, H, T>
where
    H: Handler<T> + Clone + 'static,
{
    fn call(&mut self) -> ResolveResult {
        self.handler.clone().call(self.context)
    }
}

pub trait Handler<T>: Clone {
    fn call(self, ctx: &mut FunctionContext) -> ResolveResult;
}
