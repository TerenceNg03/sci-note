// Modules to control application life and create native browser window
const {app, BrowserWindow, ipcMain, dialog} = require('electron')
const path = require('node:path')
const child_process = require('child_process')

function createWindow(port) {
    // Create the browser window.
    const mainWindow = new BrowserWindow({
        width: 1100,
        height: 700,
        minWidth: 1100,
        minHeight: 700,
        webPreferences: {
            preload: path.join(__dirname, 'preload.js'),
            contextIsolation: false,
            nodeIntegration: true
        }
    })

    mainWindow.loadURL("http://localhost:" + port)

    // Open the DevTools.
    // mainWindow.webContents.openDevTools()
}

ipcMain.on('open-file', (event, data) => {
    dialog.showOpenDialog(null, data).then(response => {
        event.sender.send('select-paper', response);
    });
})

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.whenReady().then(() => {
    console.log(process.argv[2]);
    if (process.argv[2] === "dev") {
        spawnHaskell = false
    } else {
        spawnHaskell = true
    }

    if (spawnHaskell) {
        basePath = path.join(process.resourcesPath, "app/")
        scotty = child_process.spawn(
            path.resolve(basePath, './bin/sci-note-exe'),
            [path.resolve(basePath, './build/')],
        );
        scotty.stdout.on('data', (data) => {
            process.stdout.write(data);
        });
        scotty.stderr.on('data', (data) => {
            port = parseInt(data);
            console.log("Haskell server port is", port)
            createWindow(port);
            scotty.stderr.on('data', (data) => {
                process.stderr.write(data)
            });
            app.on('activate', function () {
                // On macOS it's common to re-create a window in the app when the
                // dock icon is clicked and there are no other windows open.
                if (BrowserWindow.getAllWindows().length === 0) createWindow(port)
            })
        });
    } else {
        createWindow(3000)
        app.on('activate', function () {
            // On macOS it's common to re-create a window in the app when the
            // dock icon is clicked and there are no other windows open.
            if (BrowserWindow.getAllWindows().length === 0) createWindow(3000)
        })
    }

})

app.on('will-quit', function () {
    if (process.argv[2] !== "dev") {
        scotty.kill()
    }
})

// Quit when all windows are closed, except on macOS. There, it's common
// for applications and their menu bar to stay active until the user quits
// explicitly with Cmd + Q.
app.on('window-all-closed', function () {
    if (process.platform !== 'darwin') app.quit()
})

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.
