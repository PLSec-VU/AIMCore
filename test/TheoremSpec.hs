module TheoremSpec (simulatorTheorem, nonInterferenceTheorem) where

import Test.Tasty.QuickCheck

simulatorTheorem ::
  ( Arbitrary input
  , Arbitrary coreState
  , Show input
  , Show coreState
  , Show leakState
  , Show simState
  , Show observable
  , Show coreOutput
  , Eq observable
  , Eq leakState
  , Eq simState
  ) =>
  ((coreState, obsState) -> (leakState, simState)) ->  -- proj
  (leakState -> input -> (leakState, leakOut)) ->  -- leak circuit
  (simState -> leakOut -> (simState, observable)) ->  -- sim circuit
  (coreState -> input -> (coreState, coreOutput)) ->  -- coreCircuit
  (obsState -> coreOutput -> (obsState, observable)) ->  -- obs
  obsState ->  -- observer state
  Gen Property
simulatorTheorem proj leakCircuit simCircuit coreCircuit obs obsState = do
  input <- arbitrary
  s_core <- arbitrary
  let (s_leak, s_sim) = proj (s_core, obsState)
      (s_leak', o_leak) = leakCircuit s_leak input
      (s_sim', observable_leaksim) = simCircuit s_sim o_leak
      (s_core', o_core) = coreCircuit s_core input
      (_, observable_core) = obs obsState o_core
  pure $
    flip counterexample (observable_core == observable_leaksim && proj (s_core', obsState) == (s_leak', s_sim')) $
      unlines
        [ "input: ",
          "-------------------------------",
          show input,
          "",
          "s_core:",
          "-------------------------------",
          show s_core,
          "",
          "s_core':",
          "-------------------------------",
          show s_core',
          "",
          "s_leak:",
          "-------------------------------",
          show s_leak,
          "",
          "s_leak':",
          "-------------------------------",
          show s_leak',
          "",
          "s_sim:",
          "-------------------------------",
          show s_sim,
          "",
          "s_sim':",
          "-------------------------------",
          show s_sim',
          "",
          "proj s_core'",
          "-------------------------------",
          show $ proj (s_core', obsState),
          "",
          "observable_leaksim:",
          "-------------------------------",
          show observable_leaksim,
          "",
          "observable_core:",
          "-------------------------------",
          show observable_core,
          "",
          "o_core:",
          "-------------------------------",
          show o_core
        ]

-- | Logical implication: if p then q else True
implies :: Bool -> Bool -> Bool
implies p q = not p || q

-- | Non-interference theorem that doesn't require a simulator
-- Based on the mathematical formulation with LeakStateConsistency and LeakOutputConsistency
nonInterferenceTheorem ::
  ( Arbitrary input
  , Arbitrary coreState
  , Arbitrary obsState
  , Show input
  , Show coreState
  , Show obsState
  , Show leakState
  , Show simState
  , Show leakOut
  , Show observable
  , Eq leakState
  , Eq simState
  , Eq leakOut
  , Eq observable
  ) =>
  ((coreState, obsState) -> (leakState, simState)) ->  -- proj
  (leakState -> input -> (leakState, leakOut)) ->  -- leak circuit
  (coreState -> input -> (coreState, coreOutput)) ->  -- impl circuit
  (obsState -> coreOutput -> (obsState, observable)) ->  -- obs circuit
  Gen Property
nonInterferenceTheorem proj leak impl obs = do
  -- Test LeakStateConsistency: impl-obs-proj-state ≡ leak-proj-state
  input <- arbitrary
  s <- arbitrary
  sₒ <- arbitrary
  
  let -- Helper functions matching Agda code structure
      implObsProjState (s, sₒ) i =
        let (s', x) = impl s i
            (sₒ', _) = obs sₒ x
            (sₗ, _) = proj (s', sₒ')
        in sₗ
      
      leakProjState (s, sₒ) i =
        let (sₗ, _) = proj (s, sₒ)
            (sₗ', _) = leak sₗ i
        in sₗ'
      
      implObsProjSo ((s, sₒ), i) =
        let (s', x) = impl s i
            (sₒ', o) = obs sₒ x
            (sₗ, sₛ) = proj (s', sₒ')
        in (sₛ, o)
      
      leakProjSo ((s, sₒ), i) =
        let (sₗ, sₛ) = proj (s, sₒ)
            (sₗ', o) = leak sₗ i
        in (sₛ, o)
      
      -- LeakStateConsistency
      stateConsistency = implObsProjState (s, sₒ) input == leakProjState (s, sₒ) input
  
  -- Test LeakOutputConsistency: ∀ si si' → leak-proj-so si ≡ leak-proj-so si' → impl-obs-proj-so si ≡ impl-obs-proj-so si'
  input2 <- arbitrary
  s2 <- arbitrary
  sₒ2 <- arbitrary
  
  let si = ((s, sₒ), input)
      si' = ((s2, sₒ2), input2)
      
      -- LeakOutputConsistency: if leak outputs are equal, then impl outputs should be equal
      outputConsistency = (leakProjSo si == leakProjSo si') `implies` (implObsProjSo si == implObsProjSo si')
  
  pure $
    flip counterexample (stateConsistency && outputConsistency) $
      unlines
        [ "Non-interference theorem violation:",
          "====================================",
          "",
          "State Consistency: " ++ show stateConsistency,
          "Output Consistency: " ++ show outputConsistency,
          "",
          "input: ",
          "-------------------------------",
          show input,
          "",
          "s:",
          "-------------------------------",
          show s,
          "",
          "sₒ:",
          "-------------------------------",
          show sₒ,
          "",
          "implObsProjState result:",
          "-------------------------------",
          show $ implObsProjState (s, sₒ) input,
          "",
          "implObsProjState result:",
          "-------------------------------",
          show $ implObsProjState (s, sₒ) input,
          "",
          "leakProjState result:",
          "-------------------------------",
          show $ leakProjState (s, sₒ) input,
          "",
          "leakProjSo si:",
          "-------------------------------",
          show $ leakProjSo si,
          "",
          "leakProjSo si':",
          "-------------------------------",
          show $ leakProjSo si',
          "",
          "implObsProjSo si:",
          "-------------------------------",
          show $ implObsProjSo si,
          "",
          "implObsProjSo si':",
          "-------------------------------",
          show $ implObsProjSo si',
          "",
          "Second test case:",
          "input2: " ++ show input2,
          "s2: " ++ show s2,
          "sₒ2: " ++ show sₒ2
        ]