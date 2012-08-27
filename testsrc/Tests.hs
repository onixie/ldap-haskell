{- arch-tag: Tests main file
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

module Tests(tests) where
import Test.HUnit
import Control.Exception
import LDAP

testSearchNoAttrs = TestCase $ do
    -- A sample public LDAP server
    ldap <- ldapInitialize "ldap://scripts.mit.edu/"
    r <- ldapSearch ldap (Just "ou=People,dc=scripts,dc=mit,dc=edu") LdapScopeOnelevel Nothing LDAPNoAttrs True
    evaluate (length (show r)) -- poor mans rnf
    return ()

tests = TestList [TestLabel "testSearchNoAttrs" testSearchNoAttrs
                 ]



