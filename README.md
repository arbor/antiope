# Antiope

In Greek mythology, Antiope was an Amazon, daughter of Ares and sister to Melanippe, Hippolyta, Penthesilea and possibly Orithyia, queens of the Amazons. She may have been the wife of Theseus and mother to his son Hippolytus, but differing sources claim this was Hippolyta.
So we have many more interesting names for our future projects.

In Arbor Networks, Antiope is a tiny convenience wrapper on top of [Amazonka](https://hackage.haskell.org/package/amazonka), comprehensive Haskell AWS API.

## Adapters

`Antiope.Core` module provides some adapter functions that can help you integrate
calls to `antiope-*` and `amazonka` functions that live in `MonadAWS` into your
stack.

The relevant functions are:

* `runAws`
* `runResAws`
* `runAwsThe`
* `runResAwsThe`
* `runAwsTyped`
* `runResAwsTyped`

```haskell
foo :: MonadAWS m => m StrictValue
foo = strictAntiopeFunction arguments

foo :: (MonadAWS m, MonadResource m) -> IO LazyValue
foo = lazyAntiopeFunction arguments

foo :: Env -> IO StrictValue
foo = runAws e $ strictAntiopeFunction arguments

foo :: Env -> IO LazyValue
foo = runResAws e $ lazyAntiopeFunction arguments

foo :: (HasField' "aws" e Env, MonadIO m, MonadReader e m) -> m StrictValue
foo = runAwsThe @"aws" $ strictAntiopeFunction arguments

foo :: (HasField' "aws" e Env, MonadIO m, MonadReader e m) -> m LazyValue
foo = runResAwsThe @"aws" $ lazyAntiopeFunction arguments

foo :: (hasType Env e, MonadIO m, MonadReader e m) -> m StrictValue
foo = runAwsTyped $ strictAntiopeFunction arguments

foo :: (hasType Env e, MonadIO m, MonadReader e m) -> m LazyValue
foo = runResAwsTyped $ lazyAntiopeFunction arguments
```

The general approach to using this is to try using the functions that
don't have `Res` infix first and if that doesn't compile then the function
you are calling is returning a value that potentially has unevaluate thunks.

You will need to make the decision on whether you want to terminate the resource
immediately in `runResourceT` for the one function call, drop into a `runResourceT`
block or propagate the `MonadResource` constraint depending on your desired
resource management behaviour.

## Examples

Following are some examples for various technology specific libraries.

### Antiope-Athena

To test, in `stack repl`:

```haskell
import Control.Lens
import Control.Monad.Trans.Resource
import Network.AWS
import Network.AWS.Types
import Antiope.Core
import Antiope.Athena as A

env <- newEnv Discover <&> envRegion .~ Oregon

let config = resultConfiguration "<s3 location to save the results>"
let context = queryExecutionContext & qecDatabase ?~ "<database name>"

runAws e $ A.query config context "<query string>" "<client request token>"
```

### Antiope-DynamoDB

To test, in `stack repl`:

```haskell
import Control.Lens
import Control.Monad.Trans.Resource
import Data.HashMap.Strict
import Network.AWS
import Network.AWS.Types
import Antiope.Core
import Antiope.DynamoDB as D

e <- newEnv Discover <&> envRegion .~ Oregon

let table = TableName <table_name>

runAws e $ D.dynamoQuery table $ \q -> q
  & qKeyConditionExpression ?~ "#k = :v"
  & qLimit ?~ 1
  & qExpressionAttributeValues .~ fromList [(":v", attributeValue & avS ?~ "<primary key value>")]
  & qExpressionAttributeNames  .~ fromList [("#k", "<primary key name>")]
```



