import { useLoaderData, json } from "react-router-dom";
import OrderDetails from './OrderDetails'

export default function Order() {
    const order = useLoaderData();
    return <OrderDetails order={order}/>;
}

export async function loader({request, params}) {
    const id = params.id;
    const response = await fetch('https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/pipes/' + id +'.json');

    if (!response.ok) {
        throw new Error('Something went wrong!');
    }

    if (!response.ok) {
        return json(
          {message: "Could not fetch events."},
          {status: 500});
      } else {
        return response;
      }
  
  };  
