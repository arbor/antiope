# Antiope
In Greek mythology, Antiope was an Amazon, daughter of Ares and sister to Melanippe, Hippolyta, Penthesilea and possibly Orithyia, queens of the Amazons. She may have been the wife of Theseus and mother to his son Hippolytus, but differing sources claim this was Hippolyta.
So we have many more interesting names for our future projects.

In Arbor Networks, Antiope is a tiny convenience wrapper on top of [Amazonka](https://hackage.haskell.org/package/amazonka), comprehensive Haskell AWS API.

## Antiope-Athena
To test, in `stack repl`:
```
import Control.Monad.Trans.Resource
import Network.AWS
import Network.AWS.Types
import Antiope.Athena as A

env <- newEnv Discover <&> envRegion .~ Oregon

runResourceT . runAWS env $ A.query "<s3 location to save the results>" "<database name>" "<query string>" "<client request token>"
```
