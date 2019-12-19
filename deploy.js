const Bundler = require('parcel-bundler')
const Path = require('path')
const GitHub = require('gh-pages')

process.env.NODE_ENV = 'production'

function deploy (entry) {
  const parcel = new Bundler(entry, {
    outDir: './gh-pages',
    outFile: 'index.html',
    publicUrl: '/elm-record-helpers',
    watch: false,
    cache: false,
    contentHash: false,
    minify: true,
    target: 'browser',
    hmr: false,
    sourceMaps: false
  })

  parcel.bundle().then(() => {
    GitHub.publish('gh-pages', {
      branch: 'master',
      message: ':tada: Deploy site.'
    }, err => err ? console.error(err) : console.log('🎉 Site deployed!'))
  })
}

deploy(Path.join(__dirname, './src/index.html'))
