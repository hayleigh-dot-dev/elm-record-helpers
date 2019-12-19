import 'victormono'

import { Elm } from './elm/Main.elm'
import { promisify } from 'elm-promisify'

const flags = {}
const node = document.querySelector('[data-elm-entry]')

promisify(Elm.Main, { node, flags })
  .then(app => {
    app.ports.fromElm.subscribe(msg => {
      switch (msg) {
        case 'copy-to-clipboard':
          copyOutput()
          break
      }
    })
  })

function copyOutput () {
  const el = document.querySelector('[data-output]')

  el.select()
  el.setSelectionRange(0, 99999)

  if (el.value) {
    document.execCommand('copy')
  }
}
