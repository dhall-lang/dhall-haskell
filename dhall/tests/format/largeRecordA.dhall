-- This file was tested using console NetHack version 3.6.1
--
-- To exercise all options documented in https://nethackwiki.com/wiki/Options
-- see `./unvalidated.dhall`

let types = ./../types.dhall

let defaults = ./../defaults.dhall

in    defaults.Config
    ⫽ { AUTOCOMPLETE =
          [ { enable = True, value = "zap" }
          , { enable = False, value = "annotate" }
          ]
      , acoustics = Some True
      , align = Some { enable = True, value = types.Alignment.chaotic }
      , autodescribe = Some False
      , autodig = Some False
      , AUTOPICKUP_EXCEPTION =
          [ { pickup = False, name = "chest" }
          , { pickup = True, name = "dagger" }
          ]
      , BIND =
          [ { keybinding = "!", command = "loot" }
          , { keybinding = "^v", command = "untrap" }
          , { keybinding = "M-x", command = "terrain" }
          ]
      , catname = Some "Mirri"
      , checkpoint = Some True
      , checkspace = Some True
      , clicklook = Some False
      , cmdassist = Some True
      , confirm = Some True
      , dark_room = Some False
      , disclose =
          Some
          (   defaults.Disclose
            ⫽ { inventory = Some { prompt = True, default = True }
              , attributes = Some { prompt = True, default = False }
              , monsters_killed = Some { prompt = False, default = True }
              , monsters_genocided = Some { prompt = False, default = False }
              , conduct = Some { prompt = False, default = False }
              , dungeon_overview = Some { prompt = False, default = False }
              }
          )
      , dogname = Some "Cujo"
      , extmenu = Some False
      , fixinv = Some True
      , force_invmenu = Some False
      , fruit = Some "slime mold"
      , gender = Some types.Gender.female
      , goldX = Some False
      , help = Some True
      , hilite_pet = Some False
      , hilite_pile = Some False
      , hilite_status =
            defaults.HiliteStatus
          ⫽ { gold =
                [ { color = types.Color.yellow
                  , trigger = Some types.Numeric.always
                  , attributes = None types.Attributes
                  }
                ]
            }
      , hitpointbar = Some True
      , horsename = Some "Erhir"
      , ignintr = Some False
      , implicit_uncursed = Some True
      , legacy = Some True
      , lit_corridor = Some False
      , lootabc = Some False
      , mail = Some True
      , mention_walls = Some False
      , menucolors =
          [ { regex = "blessed"
            , color = Some types.Color.cyan
            , attributes = defaults.Attributes ⫽ { bold = Some True }
            }
          ]
      , menustyle = Some types.MenuStyle.traditional
      , menu_deselect_all = Some "-"
      , menu_deselect_page = Some "\\"
      , menu_first_page = Some "^"
      , menu_headings = Some types.MenuHeadings.bold
      , menu_invert_all = Some "@"
      , menu_invert_page = Some "~"
      , menu_last_page = Some "|"
      , menu_next_page = Some ">"
      , menu_objsyms = Some False
      , menu_previous_page = Some "<"
      , menu_search = Some ":"
      , menu_select_all = Some "."
      , menu_tab_sep = Some False
      , msg_window = Some types.MsgWindow.single
      , MSGTYPE = [ types.MsgType.hide "You swap places with .*" ]
      , name = Some "Kaeru"
      , news = Some True
      , nudist = Some False
      , null = Some False
      , number_pad = Some types.NumberPad.Letters
      , packorder = Some "\")[%?+!=/(*`0_"
      , paranoid_confirmation =
          defaults.ParanoidConfirmation ⫽ { pray = Some True }
      , pettype = Some types.PetType.cat
      , pickup_burden = Some types.PickupBurden.stressed
      , pickup_thrown = Some True
      , pickup_types = Some "?!/"
      , pile_limit = Some (types.PileLimit.limit 5)
      , playmode = Some types.PlayMode.normal
      , pushweapon = Some False
      , race = Some { enable = True, value = types.Race.elf }
      , rest_on_space = Some False
      , role = Some { enable = True, value = types.Role.wizard }
      , roguesymset = Some types.SymSet.RogueEpyx
      , runmode = Some types.RunMode.walk
      , safe_pet = Some True
      , sanity_check = Some False
      , scores = { own = Some True, around = Some 2, top = Some 10 }
      , showexp = Some False
      , showrace = Some False
      , showscore = Some False
      , silent = Some True
      , sortloot = Some types.SortLoot.none
      , sortpack = Some True
      , sparkle = Some True
      , standout = Some False
      , status_updates = Some True
      , statushilites = Some 10
      , suppress_alert = Some "3.3.1"
      , symset = Some types.SymSet.DECgraphics
      , time = Some False
      , timed_delay = Some True
      , tombstone = Some True
      , toptenwin = Some False
      , travel = Some True
      , verbose = Some True
      , whatis_coord = Some types.WhatisCoord.none
      , whatis_filter = Some types.WhatisFilter.no_filtering
      , whatis_menu = Some False
      , whatis_moveskip = Some False
      , windowtype = Some "tty"
      , wizkit = Some "wizkit.txt"
      }
