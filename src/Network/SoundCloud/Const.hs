module Network.SoundCloud.Const where

clientId :: String
clientId = "934a79db328a60a0ea459ab9e45c1735"

apiURL :: String
apiURL = "http://api.soundcloud.com"

apiURLS :: String
apiURLS = "https://api.soundcloud.com"

apiMeURLS :: String
apiMeURLS = apiURLS ++ "/me"

authURLS :: String
authURLS = "https://soundcloud.com/connect"

tokenURLS :: String
tokenURLS = apiURLS ++ "/oauth2/token"

tracksURL :: String
tracksURL = apiURL ++ "/tracks"

usersURL :: String
usersURL = apiURL ++ "/users"

playlistsURL :: String
playlistsURL = apiURL ++ "/playlists"

groupsURL :: String
groupsURL = apiURL ++ "/groups"

commentsURL :: String
commentsURL = apiURL ++ "/comments"

meConnectionsURLS :: String
meConnectionsURLS = apiMeURLS ++ "/connections"

meActivitiesURLS :: String
meActivitiesURLS = apiMeURLS ++ "/activities"

appsURLS :: String
appsURLS = apiURL ++ "/apps"

resolveURL :: String
resolveURL = apiURL ++ "/resolve"
