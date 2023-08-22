import { useNavigate } from 'react-router-dom';
import { useRef, useState } from 'react';
import { Form } from 'react-router-dom';
import classes from './OrderForm.module.css';

function OrderForm(props) {
  const navigate = useNavigate();
  const [orderItems, setOrderItems] = useState([]);
  const [addingItem, setAddingItem] = useState(false);
  const [editing, setEditingOrder] = useState(false);

  const itemDiameterRef = useRef();
  const itemQuantityRef = useRef();
  const orderDetailsRef = useRef({
    orderNumber: '',
    orderItems: []
  });

  function cancelHandler() {
    navigate('/orders');
  };


  function addNewItemHandler () {
    if (itemDiameterRef.current.value === ''|| itemQuantityRef.current.value === ''){
      return;
    }
    
    const newItem = [itemDiameterRef.current.value, itemQuantityRef.current.value];
    
    const newItems = ([
      ...orderItems, newItem
    ])

    orderDetailsRef.current = {
      ...orderDetailsRef.current,
      orderItems: newItems
    }

    itemDiameterRef.current.value = null;
    itemQuantityRef.current.value = null;
    setOrderItems(newItems);
    setAddingItem(false); 
  };

  const submitHandler = () => {
    const dataToSubmit = {
      orderNumber: orderDetailsRef.current.orderNumber,
      order: orderDetailsRef.current.orderItems
    };
    props.onSubmit(dataToSubmit);
  };

  return (
    <Form method='post' className={classes.form}>
      <p>
        <label htmlFor="orderNumber">Order Number:</label>
        <input
          id="orderNumber"
          type="number"
          name="orderNumber"
          defaultValue={orderDetailsRef.current.orderNumber}
          required
          onChange={(event) => {
                  orderDetailsRef.current.orderNumber = event.target.value;
           }} 
        />
      </p>
      <label htmlFor="Order">Order Details:</label>
      <div>
          {!editing && <h1 className={classes.h1}>
            Current Order: {orderDetailsRef.current.orderItems.map(item => `(${item})`).join('; ')}</h1>}
          {editing && 
          <div className={classes.container}>
            <h1 className={classes.h1}>
                Current Order: 
            </h1>              
                <input htmlFor="Order"
                id="order"
                type="text"
                name="order"
                defaultValue={orderDetailsRef.current.orderItems.join('; ')}
                required
                onChange={(event) => {
                  orderDetailsRef.current.orderItems = event.target.value.split(';').map(item => item.trim());
                }}

              />     
          </div>
          }
      </div>
     
      <div className={classes.actions}>      
        {addingItem &&
          <div>
            <div className={classes.container}>
            <p className='mt-4 mr-5 text-m'>Diameter: </p>
            <input
            id="diameter"
            type="number"
            name="diameter"
            ref={itemDiameterRef}/>
            </div>
            
            <div className={classes.container}>
            <p className='mt-4 mr-5 text-m'>Quantity: </p>
            <input
            id="quantity"
            type="number"
            name="quantity"
            ref={itemQuantityRef}/>
            </div>
      
        </div>}

        {(!editing && !addingItem )&& 
          <div className={classes.actions}>
            <button type='new' onClick={() => {setAddingItem(true);}}>Add Item</button>
            <button type="editing" onClick={() => {setEditingOrder(true);}} >
              Edit order
            </button>     
          </div>
        }
        
        {(addingItem  && !editing) && 
          <div className={classes.actions}>
          <button type="new" onClick={addNewItemHandler}>
            Add
          </button> 
          <button type="button" onClick={() => {setAddingItem(false);}}>
            Cancel
          </button> 
          </div>
        }
      </div>

      <div className={classes.mainButtons}> 
      {(editing && !addingItem) && 
          <div className={classes.actions}>
           <button onClick={() => {
              setEditingOrder(false);
              }}>
            Save Edit
          </button> 
          <button type="button" onClick={() => {setEditingOrder(false);}} >
            Cancel Edit
          </button> 
          </div>
      }

        {(!addingItem && !editing) && 
          <div className={classes.actions}>
          <button type="button" onClick={cancelHandler}>
            Cancel Order
          </button> 
          <button onClick={submitHandler}>
            Save Order
          </button> 
          </div>}
      </div>
 
    </Form>
  );
}

export default OrderForm;
