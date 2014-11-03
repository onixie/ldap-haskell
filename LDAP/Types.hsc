{-  -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005, 2014 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Types
   Copyright  : Copyright (C) 2005-2006 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Basic types for LDAP programs.

Written by John Goerzen, jgoerzen\@complete.org

See also "LDAP.Data" for types relating to return codes, option codes, etc.
-}

module LDAP.Types(-- * General
                  LDAP, LDAPInt, LDAPULong,
                  CPort, CRetCode, COption, CMsgID, CAll, CScope, CAttrsOnly,
                  BERInt, BERTag, BERLen
                 )
where

import Foreign.Ptr
import Data.Word
import Data.Int
import Foreign.C.Types
import Foreign.ForeignPtr
import LDAP.TypesLL
import LDAP.Data

#if defined(mingw32_BUILD_OS)
#include "windows.h"
#include "winber.h"
#else
#include "ldap.h"
#endif

{- | Main LDAP object type.

LDAP objects are automatically unbound (and memory freed) when they are
garbage-collected by Haskell. -}
type LDAP = ForeignPtr CLDAP

{- | Convenience type so we use the correct ints for the LDAP library. -}
type LDAPInt = CInt
type LDAPULong = CULong
#if defined(mingw32_BUILD_OS)
type CPort = CULong
type CRetCode = CULong
type CMsgID = CULong
type CAll = CULong
type CScope = CULong
type CAttrsOnly = CULong
#else
type CPort = CInt
type CRetCode = CInt
type CMsgID = CInt
type CAll = CInt
type CScope = CULong
type CAttrsOnly = CULong
#endif
type COption = CInt

{- | BER type tag -}
type BERTag = #type ber_tag_t

{- | BER int type -}
type BERInt = #type ber_int_t

{- | BER length type -}
#if defined(mingw32_BUILD_OS)
type BERLen = #type unsigned int
#else
type BERLen = #type ber_len_t
#endif
