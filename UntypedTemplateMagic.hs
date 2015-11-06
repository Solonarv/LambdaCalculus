module UntypedTemplateMagic where

{-# LANGUAGE TemplateHaskell #-}

import Untyped
import UntypedTemplateBase

$(mkLiftLs 10)