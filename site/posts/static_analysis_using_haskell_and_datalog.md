---
title: Combining folds using semigroups
author: Luc Tielen
date: May 5, 2021
tags:
  - haskell
  - datalog
  - static analysis
---





TODO

intro: souffle-haskell vermelden, README, talk vermelden, nu gaan we verder!
eagle eye view: wat allemaal doen
  analyse = verzameling van feiten (zo denken wij er ook over) -> herhalen in conclusie
adhv voorbeeld (simpele analyse) -> name shadowing
geen recursion schemes, simpele functie
feiten aanmaken:
  'direct', op 1 niveau -> easy
  'indirect', op meerdere niveaus -> moeilijker
    monads
    comonads/zippers/... (to be checked)
    eigenlijk elke helper functie
dan: de analyse -> .dl file, verwijzen naar souffle-haskell README
feiten/resultaten verzamelen -> shoutout andor voor resultaten verzamelen met Applicative
Result processing (in haskell)
conclusie
  verwijzen naar dead code elimination/LVA (= dus mogelijk voor allerlei soorten analysis!)
  CTA uitproberen, laten weten op twitter
  natuurlijke aanpak, schaalt heel goed

