module App.Capability.Navigate where

import Prelude

import App.Data.Route (Route)
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import Halogen.Hooks (HookM)

class Monad m <= Navigate m where
    navigate :: Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM state action slots output m) where
    navigate = lift <<< navigate

instance navigateHookM :: Navigate m => Navigate (HookM m) where
    navigate = lift <<< navigate