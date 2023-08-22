import { json, useNavigate} from 'react-router-dom';
import OrderForm from './OrderForm';


export default function NewOrder() {
  const navigate = useNavigate();

  async function submitOrder (orderData) {
    const response = await fetch('https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/pipes.json', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(orderData),
  });


  if (!response.ok) {
    throw json({ message: 'Could not save order.' }, { status: 500 });
  }

  navigate('/orders');
  };

  return <OrderForm onSubmit={submitOrder}/>;
}
