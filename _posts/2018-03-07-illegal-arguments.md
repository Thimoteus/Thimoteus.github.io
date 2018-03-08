---
layout: post
title:  "Type-safe Polymorphic Event Handling: Or, Making Illegal Argument Combinations Unrepresentable"
permalink: /:title.html
categories: purescript
---

# Motivation

Suppose you're writing bindings to a node library that has some classes which can emit events.
You may have several FFI definitions that look like this:

```javascript
exports.onError = function (obj) {
  return function (callback) {
    return function () {
      obj.on("error", function (err) {
        callback(err)();
      }
    }
  }
}
```

```purescript
foreign import data LIB_EFFECT :: Effect

foreign import data Obj :: Type

type ErrorCallback e = Error -> Eff (le :: LIB_EFFECT | e) Unit

foreign import onError :: forall e. Obj -> ErrorCallback e -> Eff (le :: LIB_EFFECT | e) Unit
```

Let's say you have `onError` and `onSuccess`, but their callbacks look different (maybe they have different arities or take different argument types).
You could just expose those two methods for handling events, and that wouldn't be so bad.
But if you find out you need to expose many more, you may want to find a DRYer approach than having a bunch of `onEvent` functions.

Suppose you've cleaned up your FFI so that instead of exporting a bunch of event handling functions, you have one catchall:

```javascript
exports.unsafeOn = ...
```

```purescript
foreign import unsafeOn :: forall obj callback e. String -> obj -> callback -> Eff (le :: LIB_EFFECT | e) Unit
```

Unfortunately we can't safely use this from Purescript -- if we want type safety we'll still need to export a bunch of functions, like `onError = unsafeOn "error"`.

So, the goal is to have a single `on` combinator that somehow takes an event type, object type, callback type and combines them in the right way.

# First attempt

Let's try enumerating all our events.

```purescript
data Event = Error | Success | Other

eventToString :: Event -> String
eventToString = case _ of
  Error -> "error"
  Success -> "success"
  Other -> "other"
```

Then we might try something like this:

```purescript
on :: forall callback e. Event -> Obj -> callback -> Eff (le :: LIB_EFFECT | e) Unit
on ev = unsafeOn (eventToString ev)
```

The problem with this is that it requires the same callback type for each event, so this won't do.

# Second attempt

There's a standard way of turning many different types into one.
In fact, we already used it to define our `Event` type, so we'll do the same for callbacks.

```purescript
data Callback e
  = ErrorCallback (Eff.Error -> Eff (le :: LIB_EFFECT | e) Unit)
  | SuccessCallback (Result -> Eff (le :: LIB_EFFECT | e) Unit)
  | OtherCallback (Eff (le :: LIB_EFFECT | e) Unit)
```

```purescript
on2 :: forall e cb. Event -> Obj -> Callback e -> Eff (le :: LIB_EFFECT | e) Unit
on2 ev obj cb = case ev, cb of
  Error, ErrorCallback cb -> unsafeOn "error" obj cb
  Success, SuccessCallback cb -> unsafeOn "success" obj cb
  Other, OtherCallback cb -> unsafeOn "other" obj cb
  _, _ -> pure unit
```

This is terrible!
We're silently failing if the wrong callback type is associated with the wrong event, which is surprising to say the least.

```purescript
noOp :: forall e. Obj -> Eff (le :: LIB_EFFECT | e) Unit
noOp obj = on2 Success obj $ ErrorCallback \ err -> log (message err)
```

Thus we also want to be able to rule out illegal argument *combinations* and in a way that's transparent to the caller.

# Third attempt

Good for us that there's a standard way of dealing with the possibility of failure (defined in our case as passing a bad combination of arguments to the `on2` function).

```purescript
on3 :: forall e cb. Event -> Obj -> Callback e -> Maybe (Eff (le :: LIB_EFFECT | e) Unit)
on3 ev obj cb = case ev, cb of
  Error, ErrorCallback cb -> Just (unsafeOn "error" obj cb)
  Success, SuccessCallback cb -> Just (unsafeOn "success" obj cb)
  Other, OtherCallback cb -> Just (unsafeOn "other" obj cb)
  _, _ -> Nothing
```

But this is *not* ideal.
While it solves the problem of making failure explicit, it pushes validation to runtime, and this problem definitely feels like something that can be prevented at compilation.

# Fourth attempt

Now we know that we want the event to somehow determine the type of the callback.
This suggests we should use a typeclass: if we can somehow exploit the lack of an instance to mean that an event is given the wrong callback type, we've succeeded.

```purescript
class On evt obj callback | evt -> obj callback where
  on :: forall e. evt -> obj -> callback -> Eff (le :: LIB_EFFECT | e) Unit

data Success = Success

data Error = Error

data Other = Other

instance onObjError :: On Error Obj (Eff.Error -> Eff (le :: LIB_EFFECT | e) Unit) where
  on _ = unsafeOn "error"
```

Nope: `Could not match type ( le :: LIB_EFFECT | e0 ) with type ( le :: LIB_EFFECT | e01 )`

The problem here is that the `e` in the callback type is not actually the same as the `e` in the result type.

# Fifth attempt

Instead of hiding the `e` behind a quantifier in the class method, let's factor out the whole result type.

```purescript
class On evt obj callback out | evt -> obj callback out where
  on :: evt -> obj -> callback -> out

data Success = Success

data Error = Error

data Other = Other

instance onObjError :: On Error Obj (Eff.Error -> Eff (le :: LIB_EFFECT | e) Unit) (Eff (le :: LIB_EFFECT | e) Unit) where
  on _ = unsafeOn "error"
```

Success!
This finally compiles.
But why stop here when we can go type-crazy?

# Fifth attempt, alternate

```purescript
foreign import kind Event

data EventProxy (e :: Event) = EventProxy

class On (evt :: Event) obj callback out | evt -> obj callback out where
  on :: forall proxy. proxy evt -> obj -> callback -> out

foreign import data Success :: Event

foreign import data Error :: Event

foreign import data Other :: Event

instance onObjError :: On Error Obj (Eff.Error -> Eff (le :: LIB_EFFECT | e) Unit) (Eff (le :: LIB_EFFECT | e) Unit) where
  on _ = unsafeOn "error"
```

# Usage

Now we can turn this:

```purescript
main = do
  obj <- newObj config
  onSuccess obj \ result -> do
    log "Success!"
    logShow result
  onError obj \ result -> do
    log "Error :("
    log (message err)
```

into this:

```purescript
main = do
  obj <- newObj config
  on success obj \ result -> do
    log "Success!"
    logShow result
  on error obj \ err -> do
    log "Error :("
    log (message err)
  where
    success = EventProxy :: EventProxy Success
    error = EventProxy :: EventProxy Error
```

But note that the following wo(uld)n't compile:

```purescript
main = do
  obj <- newObj config
  on success obj \ err -> do
    log "Error :("
    log (message err)
  where
    success = EventProxy :: EventProxy Success
```

This is because using `success` tells the compiler the callback *must* be a success callback -- whether that means (as is defined in the instance) that it takes multiple arguments, or that its one argument isn't an `Eff.Error` (note that the use of `message` tells the compiler that the callback's argument is an error).

Or you may prefer the original Attempt 5 way:

```purescript
main = do
  obj <- newObj config
  on Success obj \ result -> do
    log "Success!"
    logShow result
  on Error obj \ err -> do
    log "Error :("
    log (message err)
```

¯\\\_(ツ)_/¯

# Conclusion

Another method for ruling out bad combinations is to use a row of phantom types to constrain what arguments your functions can take (see: [purescript-node-stream](https://pursuit.purescript.org/packages/purescript-node-streams/3.1.0/docs/Node.Stream#t:Stream)'s docs. With this approach you can't, for example, use `onData` with a type that's `Writable ()`.), and this works great when the types you're constraining "look the same" -- you can differentiate them with phantoms.
