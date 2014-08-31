{-# OPTIONS_GHC -Wall #-}

-- got this idea from http://jonebird.com/2010/02/09/deciphering-caught-signals/
-- man 7 signal

-- #include <signal.h>
-- #include <stdio.h>
-- #include <unistd.h>

-- void trap (int sig) {
-- 	printf( "HUP\n");
-- }
-- struct sigaction action;
-- char cmd[256];

-- int main(int argc, char ** argv) {
-- 	action.sa_handler = trap;
--        sigemptyset (&action.sa_mask);
--        action.sa_flags = 0;
--        sigaction (SIGHUP, &action, NULL);

--        sprintf(cmd,"ps -o cmd,caught -p %d", getpid());
--        system(cmd);
-- }

module PSSignals
   ( main
   , setSignals
   ) where

import           Data.Bits
import           Numeric
import           Safe
import           System.Environment
import           Text.Groom

--  /bin/kill --list | sed -e 's/ /\n/g'
--  1 HUP      2 INT      3 QUIT     4 ILL      5 TRAP     6 ABRT     7 BUS
--  8 FPE      9 KILL    10 USR1    11 SEGV    12 USR2    13 PIPE    14 ALRM
-- 15 TERM    16 STKFLT  17 CHLD    18 CONT    19 STOP    20 TSTP    21 TTIN
-- 22 TTOU    23 URG     24 XCPU    25 XFSZ    26 VTALRM  27 PROF    28 WINCH
-- 29 POLL    30 PWR     31 SYS

data Signal = -- NULL |
              HUP
            | INT
            | QUIT
            | ILL
            | TRAP
            | ABRT
            | BUS
            | FPE
            | KILL
            | USR1
            | SEGV
            | USR2
            | PIPE
            | ALRM
            | TERM
            | STKFLT
            | CHLD
            | CONT
            | STOP
            | TSTP
            | TTIN
            | TTOU
            | URG
            | XCPU
            | XFSZ
            | VTALRM
            | PROF
            | WINCH
            | POLL
            | PWR
            | SYS
              deriving (Enum, Read, Show)

-- runghc PSSignals.hs 9
-- runghc PSSignals.hs 0000000188014007 0000000000000000 0000000000001000
main :: IO ()
main =
 getArgs
  >>= mapM_ (putStrLn . groom . setSignals . fst . headNote "main:" . readHex)

-- setSignals 0xefb
setSignals :: Integer -> [Signal]
-- setSignals i = filter (testBit i . pred . fromEnum) [HUP .. ]
setSignals i = filter (testBit i . fromEnum) [HUP .. ]
