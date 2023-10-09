// Modules to control application life and create native browser window
const {app, BrowserWindow} = require('electron')
const path = require('node:path')
const child_process = require('child_process')

if (process.argv[2] === "dev") {
    basePath = __dirname
} else {
    basePath = path.join(process.resourcesPath, "app/")
}

function createWindow() {
    // Create the browser window.
    const mainWindow = new BrowserWindow({
        width: 800,
        height: 600,
        webPreferences: {
            preload: path.join(__dirname, 'preload.js')
        }
    })

    mainWindow.loadURL("http://localhost:3000")

    // Open the DevTools.
    // mainWindow.webContents.openDevTools()
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.whenReady().then(() => {
    console.log(process.argv[2]);
    scotty = child_process.spawn(
        path.resolve(basePath, './bin/sci-note-exe'),
        [path.resolve(basePath, './build/')],
        { stdio: ["pipe", "inherit", "inherit"] }
    );

    createWindow()

    app.on('activate', function () {
        // On macOS it's common to re-create a window in the app when the
        // dock icon is clicked and there are no other windows open.
        if (BrowserWindow.getAllWindows().length === 0) createWindow()
    })
})

app.on('will-quit', function () {
    scotty.kill()
})

// Quit when all windows are closed, except on macOS. There, it's common
// for applications and their menu bar to stay active until the user quits
// explicitly with Cmd + Q.
app.on('window-all-closed', function () {
    if (process.platform !== 'darwin') app.quit()
})

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.