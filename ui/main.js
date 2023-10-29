// Modules to control application life and create native browser window
const {app, BrowserWindow, ipcMain, dialog} = require('electron')
const path = require('node:path')
const child_process = require('child_process')
const readline = require('readline')

function launchApp(port) {
    console.log("Haskell server port is", port)
    createWindow(port);
    scotty.stderr.on('data', (data) => {
        process.stderr.write(data)
    });
    app.on('activate', function () {
        // On macOS it's common to re-create a window in the app when the
        // dock icon is clicked and there are no other windows open.
        if (BrowserWindow.getAllWindows().length === 0) createWindow(port)
    });
}

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
        },
        vibrancy: 'sidebar',
        titleBarStyle: 'hiddenInset'
    })

    mainWindow.loadURL("http://localhost:" + port)
}

ipcMain.on('open-file', (event, data) => {
    dialog.showOpenDialog(null, data).then(response => {
        event.sender.send('select-paper', response);
    });
})

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
function onReady() {
    let dev = process.argv[2] === "dev"
    if (dev) {
        basePath = __dirname
        hPort = 8888
    } else {
        basePath = path.join(process.resourcesPath, "app/")
        hPort = 0
    }

    scotty = child_process.spawn(
        path.resolve(basePath, './bin/sci-note-exe'),
        ["--dir=" + path.resolve(basePath, './build/'), "--port=" + hPort],
    );
    scotty.stdout.on('data', (data) => {
        process.stdout.write(data);
    });
    var rl = readline.createInterface({
        input: scotty.stderr,
        terminal: false,
    });
    rl.on("line", (line) => {
        rl.removeAllListeners("line")
        msg = JSON.parse(line)
        if (msg.error) {
            dialog.showMessageBoxSync({
                "message": msg.message,
                "type": "error",
                "title": msg.title,
                "detail": msg.detail,
            })
            app.quit()
        } else {
            if (dev) {
                launchApp(3000)
            } else {
                launchApp(msg.port);
            }
        }
    });
}

app.whenReady().then(onReady)

app.on('will-quit', function () {
    if (scotty !== undefined) {
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
