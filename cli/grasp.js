var GraspCLI = require('../output/Language.Grasp.CLI/index.js')

var strBuf_MUTABLE = ""

var firstOption = process.argv[2] || ''

process.stdin.setEncoding('utf8')

process.stdin.on('readable', () => {
  const chunk = process.stdin.read()
  if (chunk !== null) {
    strBuf_MUTABLE += chunk
  }
})

process.stdin.on('end', () => {
  var target = GraspCLI.compile(firstOption)(strBuf_MUTABLE)
  process.stdout.write(target)
})
