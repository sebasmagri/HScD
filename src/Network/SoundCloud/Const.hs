{- |
   Module:      Network.SoundCloud.Const
   Copyright:   (c) 2012 Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   License:     BSD3
   Maintainer:  Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   Stability:   experimental

   Constants and URLs of the SoundCloud API resources
-}

module Network.SoundCloud.Const where

-- | HScD SoundCloud API client ID
clientId :: String
clientId = "934a79db328a60a0ea459ab9e45c1735"

-- | Base URL of the SoundCloud API
apiURL :: String
apiURL = "http://api.soundcloud.com"

-- | Base URL of the SoundCloud API (Secure)
apiURLS :: String
apiURLS = "https://api.soundcloud.com"

-- | Base URL for the authenticated user resources
apiMeURLS :: String
apiMeURLS = apiURLS ++ "/me"

-- | URL for the authentication interface
authURLS :: String
authURLS = "https://soundcloud.com/connect"

-- | URL of the OAuth2 token handler
tokenURLS :: String
tokenURLS = apiURLS ++ "/oauth2/token"

-- | Base URL for Tracks
tracksURL :: String
tracksURL = apiURL ++ "/tracks"

-- | Base URL for Users
usersURL :: String
usersURL = apiURL ++ "/users"

-- | Base URL for Sets/Playlists
playlistsURL :: String
playlistsURL = apiURL ++ "/playlists"

-- | Base URL for Groups
groupsURL :: String
groupsURL = apiURL ++ "/groups"

-- | Base URL for Comments
commentsURL :: String
commentsURL = apiURL ++ "/comments"

-- | URL of the authenticated user connections
meConnectionsURLS :: String
meConnectionsURLS = apiMeURLS ++ "/connections"

-- | URL of the authenticated user activities, AKA dashboard
meActivitiesURLS :: String
meActivitiesURLS = apiMeURLS ++ "/activities"

-- | Base URL for Applications
appsURLS :: String
appsURLS = apiURL ++ "/apps"

-- | URL to the resource resolver
resolveURL :: String
resolveURL = apiURL ++ "/resolve"
