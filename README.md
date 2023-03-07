# Plutology

Explore the capabilities of Plutus with this collection of cabal
scripts, each one focusing on a specific aspect.

## 1 &nbsp; Basic

### 1.1 &nbsp; MintingPolicy

- [How to get the currency symbol of a minting policy script ?](bin/Basic/MintingPolicy/CurrencySymbol.hs)
- [How to get the size of a minting policy script ?](bin/Basic/MintingPolicy/ScriptSize.hs)
- [How to reduce the script size with a custom script context ?](bin/Basic/MintingPolicy/CustomScriptContext.hs)
- [How to mint a token ?](bin/Basic/MintingPolicy/SubmitTxConstraintsWith.hs)

### 1.2 &nbsp; EmulatorTrace

- [How to run an emulator trace on a purely off-chain contract ?](bin/Basic/EmulatorTrace/RunEmulatorTraceNoValidatorEmptySchema.hs)
- [How to run a purely off-chain contract with a schema ?](bin/Basic/EmulatorTrace/RunEmulatorTraceNoValidatorBasicSchema.hs)
- [How to run an emulator trace with a simpler trace config ?](bin/Basic/EmulatorTrace/SimpleTraceConfig.hs)
