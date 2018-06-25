var Grasp = require('../output/Main/index.js')

var strBuf_MUTABLE = ""

process.stdin.setEncoding('utf8')

process.stdin.on('readable', () => {
  const chunk = process.stdin.read()
  if (chunk !== null) {
    strBuf_MUTABLE += chunk
  }
})

process.stdin.on('end', () => {
  var g1 = Grasp.compileGraphViz(strBuf_MUTABLE)()
  process.stdout.write(g1)
})
