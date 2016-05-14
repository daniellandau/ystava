# YStävä

Telegram-botti Ylioppilaskunnan Soittajien käyttöön.

## Käyttö

Lataa Git alamoduuli

```
$ git submodule init
$ git submodule update
```

Asenna [Stack](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md). [Hanki bot-tunnus](https://core.telegram.org/bots#6-botfather) Telegrammiin.

```
$ stack build
$ export TOKEN=bot[XYZ]
$ export ICAL_URL=[linkki https://jotain.ics]
$ stack exec ystava
```

Botti puhuu vain ensimmäisessä ryhmässä jossa sille puhutaan (tallentuu tiedostoon `cache`) ja ihmisille, jotka ovat
puhuneet sille kyseisessä ryhmässä.

## Osallistuminen

Osallistuminen kehitykseen (koodi tai ehdotukset/virheilmoitukset) oon tervetullutta. Pyrin vastaamaan kaikkiin ehdotuksiin nopeasti.

## Lisenssi

Tämä ohjelma on vapaata ohjelmistoa, sitä voi käyttää ja jakaa GNU Affero General Public License -lisenssin ehtojen mukaisesti. Katso (englanninkieliset) lisenssiehdot tiedostosta [LICENSE](LICENSE).

## Kiitokset

Työnantajani [Futurice](https://github.com/futurice/) antaa minulle rahallista tukea avoimen lähdekoodin kirjoittamiseen [Spice Program -ohjelman](http://spiceprogram.org/oss-sponsorship/) kautta.
