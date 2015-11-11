{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GenericsBenchTypes where

import           Distribution.Compiler
import           Distribution.License
import           Distribution.ModuleName         hiding (main)
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.Version
import           Language.Haskell.Extension

import           GHC.Generics                    (Generic)

import           Data.Binary


deriving instance Generic Version

instance Binary Benchmark
instance Binary BenchmarkInterface
instance Binary BenchmarkType
instance Binary BuildInfo
instance Binary BuildType
instance Binary CompilerFlavor
instance Binary Dependency
instance Binary Executable
instance Binary Extension
instance Binary FlagName
instance Binary KnownExtension
instance Binary Language
instance Binary Library
instance Binary License
instance Binary ModuleName
instance Binary ModuleReexport
instance Binary ModuleRenaming
instance Binary PackageDescription
instance Binary PackageIdentifier
instance Binary PackageName
instance Binary RepoKind
instance Binary RepoType
instance Binary SourceRepo
instance Binary TestSuite
instance Binary TestSuiteInterface
instance Binary TestType
instance Binary VersionRange
