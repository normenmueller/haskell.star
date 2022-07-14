# Why does `warp` require `YesodDispatch`?

```
warp
  --> toWaiAppLogger :: YesodDispatch site => Logger -> site -> IO W.Application
        --> toWaiAppYre :: YesodDispatch site => YesodRunnerEnv site -> W.Application
              --> YesodDispatch#yesodDispatch :: YesodRunnerEnv site -> W.Application
                                ^^^^^^^^^^^^^
  --> Network.Wai.Handler.Warp.runSettings :: Settings -> Application -> IO ()
```

# Why does `Application` module have this name?

The module name is a bit confusing at first. Why is this module the application? If we look at the imports `Foundation`, `Add` and `Home`, the naming becomes clearer:

In the `Application` module
- the basic data type `App` underlying the application,
- the resources 'Home' and 'Add', and
- their corresponding handlers

are merged with the `dispatch` function (cf. front control pattern).

And exactly these components essentially represent the application. As an alternative name for this module you can also imagine `Dispatch`.

# Why does `addScript[Remote]` add `<script>` at the bottom of `<body>`?

Placing scripts at the bottom of the <body> element improves the display speed, because script interpretation slows down the display (<http://bit.ly/2LNakfX>).

# Can you show me the generated code of `mkYesodData`?

In case of `yesod-minimal` w/ static assets

```haskell
src/Foundation.hs:16:1-28: Splicing declarations
    staticFiles "assets/static/"
  ======>
    styles_main_css :: StaticRoute
    styles_main_css
      = (StaticRoute
           ((map text-1.2.3.1:Data.Text.pack) ["styles", "main.css"]))
          [(text-1.2.3.1:Data.Text.pack "etag",
            text-1.2.3.1:Data.Text.pack "-PZPiv62")]

src/Foundation.hs:18:21-44: Splicing expression
    parseRoutesFile "routes"
  ======>
    [Yesod.Routes.TH.Types.ResourceLeaf
       (((((Yesod.Routes.TH.Types.Resource "HomeR") [])
            ((Yesod.Routes.TH.Types.Methods Nothing) ["GET"]))
           [])
          True),
     Yesod.Routes.TH.Types.ResourceLeaf
       (((((Yesod.Routes.TH.Types.Resource "AddR")
             [Yesod.Routes.TH.Types.Static "add",
              Yesod.Routes.TH.Types.Dynamic "Int",
              Yesod.Routes.TH.Types.Dynamic "Int"])
            ((Yesod.Routes.TH.Types.Methods Nothing) ["GET"]))
           [])
          True),
     Yesod.Routes.TH.Types.ResourceLeaf
       (((((Yesod.Routes.TH.Types.Resource "StaticR")
             [Yesod.Routes.TH.Types.Static "static"])
            ((Yesod.Routes.TH.Types.Subsite "Static") "appStatic"))
           [])
          True)]

src/Foundation.hs:18:1-45: Splicing declarations
    mkYesodData
      "App"
      ([Yesod.Routes.TH.Types.ResourceLeaf
          (((((Yesod.Routes.TH.Types.Resource "HomeR") [])
               ((Yesod.Routes.TH.Types.Methods Nothing) ["GET"]))
              [])
             True),
        Yesod.Routes.TH.Types.ResourceLeaf
          (((((Yesod.Routes.TH.Types.Resource "AddR")
                [Yesod.Routes.TH.Types.Static "add",
                 Yesod.Routes.TH.Types.Dynamic "Int",
                 Yesod.Routes.TH.Types.Dynamic "Int"])
               ((Yesod.Routes.TH.Types.Methods Nothing) ["GET"]))
              [])
             True),
        Yesod.Routes.TH.Types.ResourceLeaf
          (((((Yesod.Routes.TH.Types.Resource "StaticR")
                [Yesod.Routes.TH.Types.Static "static"])
               ((Yesod.Routes.TH.Types.Subsite "Static") "appStatic"))
              [])
             True)])
  ======>
    instance ParseRoute App where
      parseRoute
        = ((\ f_aaXD x_aaXE -> (f_aaXD ()) x_aaXE) ::
             (()
              -> ([text-1.2.3.1:Data.Text.Internal.Text],
                  [(text-1.2.3.1:Data.Text.Internal.Text,
                    text-1.2.3.1:Data.Text.Internal.Text)])
                 -> Maybe (Route a_aaXF))
             -> ([text-1.2.3.1:Data.Text.Internal.Text],
                 [(text-1.2.3.1:Data.Text.Internal.Text,
                   text-1.2.3.1:Data.Text.Internal.Text)])
                -> Maybe (Route a_aaXF))
            helper_aaXC
        where
            helper_aaXC env5004_aaXk req5004_aaXl
              = helper5004_aaXm (fst req5004_aaXl)
              where
                  helper5004_aaXm []
                    = ((((\ _ _ x_aaXo _ -> x_aaXo) (error "mdsGetHandler"))
                          env5004_aaXk)
                         (Just HomeR))
                        req5004_aaXl
                  helper5004_aaXm
                    ((:) "add"
                         ((:) (fromPathPiece -> Just dyn_aaXp)
                              ((:) (fromPathPiece -> Just dyn_aaXq) [])))
                    = ((((\ _ _ x_aaXr _ -> x_aaXr)
                           (((error "mdsGetHandler") dyn_aaXp) dyn_aaXq))
                          env5004_aaXk)
                         (Just ((AddR dyn_aaXp) dyn_aaXq)))
                        req5004_aaXl
                  helper5004_aaXm ((:) "static" restPath_aaXs)
                    = (((((\ _runHandler_aaXv _getSub_aaXw toMaster_aaXx _env_aaXy
                             -> (fmap toMaster_aaXx . parseRoute))
                            (\ _ _ x_aaXz _ -> x_aaXz))
                           (\ sub_aaXA -> appStatic sub_aaXA))
                          (\ sroute_aaXB -> StaticR sroute_aaXB))
                         env5004_aaXk)
                        (((\ p_aaXt (_, q_aaXu) -> (p_aaXt, q_aaXu)) restPath_aaXs)
                           req5004_aaXl)
                  helper5004_aaXm _
                    = ((((\ _ _ x_aaXn _ -> x_aaXn) (error "mds404")) env5004_aaXk)
                         Nothing)
                        req5004_aaXl
    instance RenderRoute App where
      data Route App
        = HomeR | AddR Int Int | StaticR (Route Static)
        deriving (Show, Eq, Read)
      renderRoute HomeR = ([], [])
      renderRoute (AddR dyn_aaX1 dyn_aaX2)
        = ((text-1.2.3.1:Data.Text.pack "add"
              : (toPathPiece dyn_aaX1 : (toPathPiece dyn_aaX2 : []))),
           [])
      renderRoute (StaticR sub_aaX3)
        = (\ (a_aaX4, b_aaX5)
             -> ((text-1.2.3.1:Data.Text.pack "static" : a_aaX4), b_aaX5))
            (renderRoute sub_aaX3)
    instance RouteAttrs App where
      routeAttrs HomeR {}
        = containers-0.6.0.1:Data.Set.Internal.fromList []
      routeAttrs AddR {}
        = containers-0.6.0.1:Data.Set.Internal.fromList []
      routeAttrs StaticR {}
        = containers-0.6.0.1:Data.Set.Internal.fromList []
    resourcesApp :: [Yesod.Routes.TH.Types.ResourceTree String]
    resourcesApp
      = [Yesod.Routes.TH.Types.ResourceLeaf
           (((((Yesod.Routes.TH.Types.Resource "HomeR") [])
                ((Yesod.Routes.TH.Types.Methods Nothing) ["GET"]))
               [])
              True),
         Yesod.Routes.TH.Types.ResourceLeaf
           (((((Yesod.Routes.TH.Types.Resource "AddR")
                 [Yesod.Routes.TH.Types.Static "add",
                  Yesod.Routes.TH.Types.Dynamic "Int",
                  Yesod.Routes.TH.Types.Dynamic "Int"])
                ((Yesod.Routes.TH.Types.Methods Nothing) ["GET"]))
               [])
              True),
         Yesod.Routes.TH.Types.ResourceLeaf
           (((((Yesod.Routes.TH.Types.Resource "StaticR")
                 [Yesod.Routes.TH.Types.Static "static"])
                ((Yesod.Routes.TH.Types.Subsite "Static") "appStatic"))
               [])
              True)]
    type Handler = HandlerFor App
    type Widget = WidgetFor App ()
```

So, `mkYesodData` generates three instances, `PraseRoute`, `RenderRoute`, and `RouteAttrs`, one function `resourcesApp`, and two type synonyms. And, actually, also the associated data type `Route`.

This also explains, why the module with `mkYesodDispatch`, usually called `Application` or `Disptach`, must have the following import declaration:

```haskell
import Foundation (<Foundation>(..), Route(..), resources<Foundation>)
```

The instances are imported implicitly.

# Why does `addScript` parameterized with a static route compile?

`addScript` is declared as `addScript :: MonadWidget m => Route (HandlerSite m) -> m ()`. So, why does `addScript $ StaticR (scripts_main_js :: StaticRoute)` compile? Or, perhaps, why does `addScript $ (scripts_main_js :: Route Static)` *not* compile?

To begin with, `staticFiles` generates `scripts_main_js :: StaticRoute`, whereas `type StaticRoute = Route Static`.

Next, let's check how the associate data type `Route` is defined:

```haskell
instance RenderRoute Yello where
  data Route Yello
    = HomeR
    | StaticR (Route Static)
```

We can see that `HomeR` and `StaticR` are the only value constructors. That's why `addScript $ (scripts_main_js :: Route Static)` does *not* compile.

So, let's back to the question why does `addScript $ StaticR scripts_main_js` compile? Well, obviously the types just match.

>N.B., `Static` is a single-field variant of `Static StaticSettings`: `newtype Static = Static StaticSettings`. So, to put it casually, whenever we see a `Static`, we can think of a `StaticSettings`. And a `StaticSettings` is "just" a data type w/o forming any abstract concept, i.e., w/o being an instance of any type class.
>  ```haskell
> -- Network.Wai.Application.Static
> data StaticSettings = StaticSettings { ... }
> ```
> For the sake of completeness, `HandlerSite` is an associate data type of `MonadHandler`.`

# Why does `defaultLayout` applied to `setTitle` compile?

```haskell
defaultLayout :: WidgetFor site () -> HandlerFor site Html
setTitle      :: MonadWidget m => Html -> m ()
```

`setTitle` does not return *some* instance it chooses, it can return *any* instance the *caller* chooses. All type variables like `m` in Haskell are implicitly quantified universally---this means *the caller chooses what they are*. E.g. `Nothing :: Maybe a` means you can use `Nothing` where you need `Maybe Int`, choosing `a=Int`, but also where you need `Maybe String`, etc. It's a "for all m" vs "for some m" distinction, from a logical view.

```
setTitle :: MonadWidget m => Html -> m ()
```

reads as the following contract between the *caller* and the implementation of the function

- the caller has to choose `m`
- the caller must ensure that the chosen `m` satisfies the `MonadWidget m` constraint
- the caller must pass a `Html` argument
- the function will return a value of type `m ()`

Note that it is not `setTitle` to choose `m`. The function is polymorphic (or "generic" in OOP lingo), and works at any `m` the caller chooses.

Since `defaultLayout` requires `m = WidgetFor site`, GHC infers that monad to be used at the `setTitle` call. Everything then type checks.

Cf. <http://bit.ly/2Yjo4km>
