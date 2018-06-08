# Antiope
In Greek mythology, Antiope was an Amazon, daughter of Ares and sister to Melanippe, Hippolyta, Penthesilea and possibly Orithyia, queens of the Amazons. She may have been the wife of Theseus and mother to his son Hippolytus, but differing sources claim this was Hippolyta.
So we have many more interesting names for our future projects.

In Arbor Networks, Antiope is a tiny convenience wrapper on top of [Amazonka](https://hackage.haskell.org/package/amazonka), comprehensive Haskell AWS API.

## Antiope-Athena
To test, in `stack repl`:
```
import Control.Lens
import Control.Monad.Trans.Resource
import Network.AWS
import Network.AWS.Types
import Antiope.Athena as A

env <- newEnv Discover <&> envRegion .~ Oregon

let config = resultConfiguration "<s3 location to save the results>"
let context = queryExecutionContext & qecDatabase ?~ "<database name>"

runResourceT . runAWS env $ A.query config context "<query string>" "<client request token>"
```

## Antiope-DynamoDB
To test, in `stack repl`:
```
import Control.Lens
import Control.Monad.Trans.Resource
import Data.HashMap.Strict
import Network.AWS
import Network.AWS.Types
import Antiope.DynamoDB as D

env <- newEnv Discover <&> envRegion .~ Oregon

let table = TableName <table_name>

runResourceT . runAWS env $ D.dynamoQuery table $ \q -> q & qKeyConditionExpression ?~ "#k = :v" & qLimit ?~ 1 & qExpressionAttributeValues .~ fromList [(":v", attributeValue & avS ?~ "<primary key value>")]   & qExpressionAttributeNames  .~ fromList [("#k", "<primary key name>")]
```
