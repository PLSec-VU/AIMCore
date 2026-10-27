module Proof.SMT.Bench.Memcmp
  ( memcmpEntry
  , memcmpCode
  , sodiumMemcmpEntry
  , sodiumMemcmpCode
  ) where

import Stepper.RelState (Code, noCode, withInstruction)
import Types (Address)

memcmpEntry :: Address
memcmpEntry = 0x427ec

memcmpCode :: Code
memcmpCode =
  withInstruction 0x427ec 0x00300693 $
  withInstruction 0x427f0 0x04c6fe63 $
  withInstruction 0x427f4 0x00a5e7b3 $
  withInstruction 0x427f8 0x0037f793 $
  withInstruction 0x427fc 0x02078a63 $
  withInstruction 0x42800 0xfff60613 $
  withInstruction 0x42804 0x00160613 $
  withInstruction 0x42808 0x00c50633 $
  withInstruction 0x4280c 0x0080006f $
  withInstruction 0x42810 0x04a60063 $
  withInstruction 0x42814 0x00054783 $
  withInstruction 0x42818 0x0005c703 $
  withInstruction 0x4281c 0x00150513 $
  withInstruction 0x42820 0x00158593 $
  withInstruction 0x42824 0xfee786e3 $
  withInstruction 0x42828 0x40e78533 $
  withInstruction 0x4282c 0x00008067 $
  withInstruction 0x42830 0x00052703 $
  withInstruction 0x42834 0x0005a783 $
  withInstruction 0x42838 0xfcf714e3 $
  withInstruction 0x4283c 0xffc60613 $
  withInstruction 0x42840 0x00450513 $
  withInstruction 0x42844 0x00458593 $
  withInstruction 0x42848 0xfec6e4e3 $
  withInstruction 0x4284c 0xfa061ae3 $
  withInstruction 0x42850 0x00000513 $
  withInstruction 0x42854 0x00008067 $
    noCode

sodiumMemcmpEntry :: Address
sodiumMemcmpEntry = 0x11c40

sodiumMemcmpCode :: Code
sodiumMemcmpCode =
  withInstruction 0x11c40 0xfd010113 $
  withInstruction 0x11c44 0x02812423 $
  withInstruction 0x11c48 0x02912223 $
  withInstruction 0x11c4c 0x00c12623 $
  withInstruction 0x11c50 0x02112623 $
  withInstruction 0x11c54 0x00010fa3 $
  withInstruction 0x11c58 0x00050493 $
  withInstruction 0x11c5c 0x00058413 $
  withInstruction 0x11c60 0xfddff0ef $
  withInstruction 0x11c64 0x00c12603 $
  withInstruction 0x11c68 0x02060a63 $
  withInstruction 0x11c6c 0x00048513 $
  withInstruction 0x11c70 0x00c48633 $
  withInstruction 0x11c74 0x00040593 $
  withInstruction 0x11c78 0x00054703 $
  withInstruction 0x11c7c 0x0005c683 $
  withInstruction 0x11c80 0x01f14783 $
  withInstruction 0x11c84 0x00150513 $
  withInstruction 0x11c88 0x00d74733 $
  withInstruction 0x11c8c 0x00e7e7b3 $
  withInstruction 0x11c90 0x00f10fa3 $
  withInstruction 0x11c94 0x00158593 $
  withInstruction 0x11c98 0xfea610e3 $
  withInstruction 0x11c9c 0x01f14503 $
  withInstruction 0x11ca0 0x02c12083 $
  withInstruction 0x11ca4 0x02812403 $
  withInstruction 0x11ca8 0xfff50513 $
  withInstruction 0x11cac 0x01f55513 $
  withInstruction 0x11cb0 0x02412483 $
  withInstruction 0x11cb4 0xfff50513 $
  withInstruction 0x11cb8 0x03010113 $
  withInstruction 0x11cbc 0x00008067 $
  withInstruction 0x11c3c 0x00008067 $
    noCode
