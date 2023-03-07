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
- [What happen when activating the same contract again ?](bin/Basic/EmulatorTrace/TwoActivateContractNoEndpoint.hs)
- [What happen when calling endpoints on another contract instance ?](bin/Basic/EmulatorTrace/TwoActivateContractOnEndpoint.hs)
- [How the EmulatorTrace can know the state of a Contract ?](bin/Basic/EmulatorTrace/ObservableStateNoEndpoint.hs)
- [How the EmulatorTrace can avoid missed callEndpoint ?](bin/Basic/EmulatorTrace/EndpointBasicScheduler.hs)
