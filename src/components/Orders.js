import { useLoaderData , json } from "react-router-dom";
import OrderList from "./OrderList";
import classes from './OrderList.module.css';
import { Link } from "react-router-dom";

export default function Orders(){

    const data = useLoaderData();
    const orders = [] 


    for (const key in data){
        orders.push({
            id: key, 
            orderNumber: data[key].orderNumber,
            order: data[key].order
        })
    };
    
    return (
        <>
            <div >
                <OrderList orders={orders}/>
                <div className={classes.actions}>
                    <button type="button">
                        <Link to='/new'>Add new order</Link>
                        </button>
                </div>
            </div>
        </>
       
    );
};

export async function loader() {
        const response = await fetch('https://pipenesting-default-rtdb.europe-west1.firebasedatabase.app/pipes.json');

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
