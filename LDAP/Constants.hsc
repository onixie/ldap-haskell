{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005, 2014 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Constants
   Copyright  : Copyright (C) 2005-2006 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

LDAP constants for use in your programs

Written by John Goerzen, jgoerzen\@complete.org
-}

module LDAP.Constants(module LDAP.Constants)
where
import Foreign.C.Types
import LDAP.Types

#if !defined(mingw32_BUILD_OS)
#include "ldap.h"
#else
#include "windows.h"
#include "winldap.h"
#include "winber.h"
#endif

#enum CPort, ,LDAP_PORT

#enum LDAPInt, , LDAP_API_VERSION, LDAP_VENDOR_NAME, \
      LDAP_API_INFO_VERSION, LDAP_FEATURE_INFO_VERSION, \
      LDAP_CONTROL_VLVREQUEST, LDAP_CONTROL_VLVRESPONSE

#enum BERTag, , LDAP_FILTER_AND, LDAP_FILTER_OR, LDAP_FILTER_NOT, \
      LDAP_FILTER_EQUALITY, LDAP_FILTER_SUBSTRINGS, LDAP_FILTER_GE,\
      LDAP_FILTER_LE, LDAP_FILTER_PRESENT, LDAP_FILTER_APPROX,\
      LDAP_SUBSTRING_ANY, LDAP_SUBSTRING_FINAL, LDAP_SUBSTRING_INITIAL

#if !defined(mingw32_BUILD_OS)
#enum CPort, , LDAPS_PORT
#enum LDAPInt, , LDAP_CONTROL_VALUESRETURNFILTER, LDAP_CONTROL_SUBENTRIES, \
      LDAP_CONTROL_NOOP, LDAP_CONTROL_MANAGEDSAIT, LDAP_CONTROL_PROXY_AUTHZ, \
      LDAP_CONTROL_SORTREQUEST, LDAP_CONTROL_SORTRESPONSE, \
      LDAP_NOTICE_OF_DISCONNECTION, LDAP_NOTICE_DISCONNECT
#enum BERTag, , LDAP_FILTER_EXT, LDAP_FILTER_EXT_OID, LDAP_FILTER_EXT_TYPE,\
      LDAP_FILTER_EXT_VALUE,LDAP_FILTER_EXT_DNATTRS
#else
#enum CPort, , LDAP_SSL_PORT
#endif
