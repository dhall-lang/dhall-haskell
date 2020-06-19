{ pull_request_rules =
  [ { actions =
      { backport = None { branches : List Text }
      , delete_head_branch = None {}
      , label = None { remove : List Text }
      , `merge` = Some { method = "squash", strict = "smart" }
      }
    , conditions =
      [ "status-success=continuous-integration/appveyor/pr"
      , "label=merge me"
      , "#approved-reviews-by>=1"
      ]
    , name = "Automatically merge pull requests"
    }
  , { actions =
      { backport = None { branches : List Text }
      , delete_head_branch = Some {=}
      , label = None { remove : List Text }
      , `merge` = None { method : Text, strict : Text }
      }
    , conditions = [ "merged" ]
    , name = "Delete head branch after merge"
    }
  , { actions =
      { backport = Some { branches = [ "1.0.x" ] }
      , delete_head_branch = None {}
      , label = Some { remove = [ "backport-1.0" ] }
      , `merge` = None { method : Text, strict : Text }
      }
    , conditions = [ "merged", "label=backport-1.0" ]
    , name = "backport patches to 1.0.x branch"
    }
  , { actions =
      { backport = Some { branches = [ "1.1.x" ] }
      , delete_head_branch = None {}
      , label = Some { remove = [ "backport-1.1" ] }
      , `merge` = None { method : Text, strict : Text }
      }
    , conditions = [ "merged", "label=backport-1.1" ]
    , name = "backport patches to 1.1.x branch"
    }
  , { actions =
      { backport = Some { branches = [ "1.2.x" ] }
      , delete_head_branch = None {}
      , label = Some { remove = [ "backport-1.2" ] }
      , `merge` = None { method : Text, strict : Text }
      }
    , conditions = [ "merged", "label=backport-1.2" ]
    , name = "backport patches to 1.2.x branch"
    }
  , { actions =
      { backport = Some { branches = [ "1.3.x" ] }
      , delete_head_branch = None {}
      , label = Some { remove = [ "backport-1.3" ] }
      , `merge` = None { method : Text, strict : Text }
      }
    , conditions = [ "merged", "label=backport-1.3" ]
    , name = "backport patches to 1.3.x branch"
    }
  , { actions =
      { backport = Some { branches = [ "1.4.x" ] }
      , delete_head_branch = None {}
      , label = Some { remove = [ "backport-1.4" ] }
      , `merge` = None { method : Text, strict : Text }
      }
    , conditions = [ "merged", "label=backport-1.4" ]
    , name = "backport patches to 1.4.x branch"
    }
  , { actions =
      { backport = Some { branches = [ "1.5.x" ] }
      , delete_head_branch = None {}
      , label = Some { remove = [ "backport-1.5" ] }
      , `merge` = None { method : Text, strict : Text }
      }
    , conditions = [ "merged", "label=backport" ]
    , name = "backport patches to 1.5.x branch"
    }
  ]
}
