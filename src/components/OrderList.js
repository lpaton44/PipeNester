import classes from './OrderList.module.css';
import { Link } from 'react-router-dom';

export default function OrderList({ orders }) {
    
    function getListItems(order){
        return order.map((orderItem) => {
            return <li>({orderItem[0]}, {orderItem[1]})</li>;});
    };

    return (
        <>
        <h1 className = {classes.h1}>All Orders</h1>
        <div className="flex items-center justify-center h-full">
        <div className="w-full max-w-screen-md">
            <table className="min-w-full py-2 text-center table-auto text-x-large">
                <thead className="border-b font-large dark:border-neutral-500">
                    <tr>
                        <th >Order Number</th>
                        <th >Order Summary</th>
                    </tr>
                </thead>
                <tbody>
                    {
                        orders.map((orderItem, index) => (
                        <tr key={orderItem.id} 
                        className={`${index % 2 === 0 ? 'bg-gray-100' : 'bg-white'} border-b dark:border-neutral-500`}>
                            <td ><Link to={'/orders/' + orderItem.id}>{orderItem.orderNumber}</Link></td>
                            <td ><Link to={'/orders/' + orderItem.id}>
                                <ul>
                                    {getListItems(orderItem.order)}
                                </ul>
                            </Link></td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </div> 
    </div>
    </>  
);}