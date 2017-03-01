module HLint.HLint where

import "hint" HLint.Default
import "hint" HLint.Builtin.All
import "hint" HLint.Dollar
-- import "hint" HLint.Generalise

warn = return ==> pure
warn = (>>) ==> (*>)
warn = liftIO . atomically ==> atomicallyIO
