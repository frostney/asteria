Asteria
=======

Prototyping framework for JavaScript, that works on the desktop

What does it do?
---
Asteria provides a framework and an IDE to rapidly prototype games.

Features
---

__Core:__  

* _require_ function very similar to CommonJS (allows JSON reading and _index.js_ in subfolders)  
* Console object by Dmitry A. Soshnikov
* ES6 shim by Paul Miller
* JSON.stringify and JSON.parse provided through BESEN

__Window:__

* Window is the global object
* _window.innerHeight_
* _window.innerWidth_
* _window.setTimeout_ (in progress)
* _window.setInterval_ (in progress)
* _window.requestAnimationFrame_ (in progress)

__Document:__

* _document.addEventListener_ (in progress)
* _document.getElementById_ (in progress)

__Filesystem:__

* Similar to Node.js _fs_ Object
* All operations are done synchronously
* _fs.fileExists_: Checks if a file exists
* _fs.dirExists_: Checks if a directory exists
* _fs.readFile_: Reads a file (only text is supported)
* _fs.writeFile_: Writes a file (only text is supported)

Eeek! Why are there binaries and dynamic libaries in the repository?
---

Well, they are there so you can try it for yourself without needing to compile the source for yourself.

Used libraries
--
* Slightly modified version of BESEN (http://besen.sourceforge.net)
* ES6 shim