var cluster = require('cluster')
  , http = require('http')

module.exports = function (n, m) {
  if (cluster.isMaster) {
    console.time('in')
    var worker, ni = n, mi = m
    while (ni--) worker = new Worker(worker)
    while (mi--) worker.hello()

    cluster.on('exit', function (worker, code, signal) {
      if (!Object.keys(cluster.workers).length) {
        console.log('%d messages sent through ring', n * m)
        console.timeEnd('in')
      }
    })
  } else {
    var mCount = 0
    process.on('message', function (msg) {
      if (++mCount >= m) {
        cluster.worker.kill()
      }
    })
  }
}

function Worker(next) {
  var me = cluster.fork()
  me.hello = function () {
    me.send('hello')
    if (next) {
      next.hello()
    }
  }
  return me
}
