module Icons exposing (calendarIcon, chevronLeftIcon, chevronRightIcon, homeIcon, loginIcon, logoutIcon, mapIcon)

import Color
import Html exposing (Html)
import Material.Icons.Action as ActionIcons
import Material.Icons.Navigation as NavigationIcons


loginIcon : Html msg
loginIcon =
    ActionIcons.account_circle Color.black 16


logoutIcon : Html msg
logoutIcon =
    ActionIcons.exit_to_app Color.black 16


homeIcon : Html msg
homeIcon =
    ActionIcons.home Color.black 16


calendarIcon : Html msg
calendarIcon =
    ActionIcons.event Color.black 16


mapIcon : Html msg
mapIcon =
    ActionIcons.room Color.black 16


chevronLeftIcon : Html msg
chevronLeftIcon =
    NavigationIcons.chevron_left Color.black 16


chevronRightIcon : Html msg
chevronRightIcon =
    NavigationIcons.chevron_right Color.black 16