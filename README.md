# sci-note

Manage your papers. Build with Haskell, ReactJS & Electron.


### Build this Repo

Make sure you have `npm` and `stack` (recommended to be installed via `ghcup`) installed.

Run the following script:
```sh
stack build --copy-bins #build scotty server
cd ui
npm install             #install npm libraries
npm run build           #build react app
npm run package         #package into electron app
```

Your app should be located at `./ui/out/Sci Note-<OS_NAME>/`.

### Uninstall/Transfer Data
All data is stored at `$HOME/sci-note/`(unix) or `C:/Users/<user>/Documents/sci-note/`(windows).
