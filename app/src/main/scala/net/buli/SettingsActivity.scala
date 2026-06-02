package net.buli

import android.widget.{LinearLayout, TextView}
import fr.acinq.eclair._
import immortan.utils._

class SettingsActivity extends BaseCheckActivity
  with HasTypicalChainFee with ChoiceReceiver {

  private[this] lazy val settingsContainer = findViewById(R.id.settingsContainer).asInstanceOf[LinearLayout]
  private[this] lazy val titleText = findViewById(R.id.titleText).asInstanceOf[TextView]

  private[this] val fiatSymbols = LNParams.fiatRates.universallySupportedSymbols.toList.sorted
  private[this] val CHOICE_FIAT_DENOMINATION_TAG = "choiceFiatDenomination"
  private[this] val CHOICE_BTC_DENOMINATION_TAG = "choiceBtcDenomination"
  private[this] val units = List(SatDenomination, BtcDenomination)

  override def onResume(): Unit = {
    super.onResume()
    setFiat.updateView()
    setBtc.updateView()
  }

  override def onChoiceMade(tag: AnyRef, pos: Int): Unit = tag match {
    case CHOICE_FIAT_DENOMINATION_TAG =>
      val fiatCode = fiatSymbols(pos)
      WalletApp.app.prefs.edit.putString(WalletApp.FIAT_CODE, fiatCode).commit()
      ChannelMaster.next(ChannelMaster.stateUpdateStream)
      setFiat.updateView()
    case CHOICE_BTC_DENOMINATION_TAG =>
      WalletApp.app.prefs.edit.putString(WalletApp.BTC_DENOM, units(pos).sign).commit()
      ChannelMaster.next(ChannelMaster.stateUpdateStream)
      setBtc.updateView()
    case _ =>
  }

  private lazy val setFiat = new SettingsHolder(this) {
    def updateView(): Unit = {
      val current = WalletApp.app.prefs.getString(WalletApp.FIAT_CODE, "USD")
      settingsTitle.setText("Fiat")
      settingsInfo.setText(current)
      setOnClickListener(_ => showChoiceDialog(fiatSymbols, CHOICE_FIAT_DENOMINATION_TAG))
    }
  }

  private lazy val setBtc = new SettingsHolder(this) {
    def updateView(): Unit = {
      val current = WalletApp.app.prefs.getString(WalletApp.BTC_DENOM, "sat")
      settingsTitle.setText("Bitcoin unit")
      settingsInfo.setText(current)
      setOnClickListener(_ => showChoiceDialog(units.map(_.sign), CHOICE_BTC_DENOMINATION_TAG))
    }
  }
}