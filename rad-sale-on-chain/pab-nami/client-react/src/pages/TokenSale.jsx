import React, { Fragment, useEffect, useState } from 'react'
import { paytoWalletAction } from '../utils/cardano';

export default function TokenSale() {

  const [walletConnected, setWalletConnected] = useState(false)

  const [payToWalletFormState, setPayToWalletFormState] = useState(
    {
      address: "addr_test1qpvagl6ns8gfe7f0w74nylkyht53hlfhsvhpgvsf45c0cr4j2ftykgcgwpq4hma4mtjupynzky5pg9p9qzalu8zu3d6qscr5vz",
      amount: 2000000
    }
  )

  useEffect(() => {
    if (window.cardano != null) {
      window.cardano.nami.enable().then(
        () => window.cardano.nami.isEnabled().then((result) => {
          setWalletConnected(result)
          if (result) {
            console.log("Calling activate endpoint")
          }
        })
      )
    }
  }, []);

  function handlePayToWalletFormSubmit () {
    paytoWalletAction(payToWalletFormState.address, payToWalletFormState.amount)
  }

  function handelPaytToWalletFormChange (event) {
    const name = event.target.name;
    let value = event.target.value;

    if (name == "amount")
      value = Number.parseInt(value)
    
    setPayToWalletFormState(values => ({...values, [name]: value}))
  }

  return (
    <Fragment>
      <div className="container">
        <h1>RAD Token sale</h1>
        <p>This is the main web interface for the RAD token sale contract.</p>
        <p>The nami wallet is: {walletConnected ? 'connected' : 'disconnected'}</p>
        <form>
          <div className="mb-3">
            <label htmlFor="sellerPubkeyHash" className="form-label">Seller Pubkey Hash</label>
            <input type="text" name="sellerPubkeyHash" id="sellerPubkeyHash" className="form-control" />
          </div>
          <div className="mb-3">
            <label htmlFor="tokenCost" className="form-label">Token cost</label>
            <input type="text" name="tokenCost" id="tokenCost" className="form-control" />
          </div>
          <div className="mb-3">
            <label htmlFor="currencySymbol" className="form-label">Currency Symbol</label>
            <input type="text" name="currencySymbol" id="currencySymbol" className="form-control" />
          </div>
          <div className="mb-3">
            <label htmlFor="tokenName" className="form-label">Token name</label>
            <input type="text" name="tokenName" id="tokenName" className="form-control" />
          </div>

          <button type="button" className="btn btn-primary me-2">Open</button>
          <button type="button" className="btn btn-secondary me-2">Buy</button>
          <button type="button" className="btn btn-success me-2">Close</button>
        </form>
        <br></br>
        <br></br>
        <h1>Plutus APP Example</h1>
        <p>This is the main web interface for the RAD token sale contract. To run this form, you need to start the pab-nami-demo-server from the plutus-app repository.</p>
        <form>
          <div className="mb-3">
            <label htmlFor="address" className="form-label">Address</label>
            <input type="text" name="address" id="address" value={ payToWalletFormState.address } onChange={ handelPaytToWalletFormChange } className="form-control" />
          </div>
          <div className="mb-3">
            <label htmlFor="amount" className="form-label">Amount</label>
            <input type="number" name="amount" id="amount" value={ payToWalletFormState.amount } onChange={ handelPaytToWalletFormChange } className="form-control" />
          </div>

          <button type="button" onClick={ handlePayToWalletFormSubmit } className="btn btn-primary me-2">Make payment</button>
        </form>
      </div>
    </Fragment>
  )
}
