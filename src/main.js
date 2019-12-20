import 'victormono'

import { Elm } from './elm/Main.elm'
import { promisify } from 'elm-promisify'

const defaultCode = [
  '-- Paste your record types here',
  '',
  'type alias Model =',
  '  { count : Int',
  '  }'
].join('\n')
const flags = window.location.hash.startsWith('#code=') ? window.atob(window.location.hash.slice(6)) : defaultCode
const node = document.querySelector('[data-elm-entry]')
const portHandlers = {
  //
  'copy-to-clipboard' () {
    const el = document.querySelector('[data-output]')

    el.select()
    el.setSelectionRange(0, 99999)

    if (el.value) {
      document.execCommand('copy')
    }
  },

  //
  'update-url' (input = '') {
    window.location.hash = input.length > 0
      ? '#code=' + window.btoa(input)
      : ''
  }
}

promisify(Elm.Main, { node, flags })
  .then(app => {
    app.ports.fromElm.subscribe(({ message, payload }) => {
      portHandlers[message](payload)
    })
  })
