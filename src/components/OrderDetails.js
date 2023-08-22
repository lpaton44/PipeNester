import React from "react";
import classes from './OrderDetails.module.css';
import { useNavigate } from "react-router-dom";

export default function OrderDetails({ order }) {
    const navigate = useNavigate();
    

    return (
        <> 
        <div className="flex items-center justify-center h-full text-center">
            <div className="w-full max-w-screen-md">
                <h1 className="mt-5 mb-4 text-2xl font-semibold tracking-tight text-center">Order Number {order.orderNumber}</h1>
                <table className="min-w-full text-center text-x-large">
                    <thead className="border-b font-large dark:border-neutral-500">
                        <tr>
                            <th>Diameter</th>
                            <th>Quantity</th>
                        </tr>
                    </thead>
                    <tbody className="text-center border-b font-large dark:border-neutral-500">
                        {order.order.map((orderItem, index) => (
                            <tr
                                key={orderItem[0]}
                                className={`${index % 2 === 0 ? 'bg-gray-100' : 'bg-white'} border-b dark:border-neutral-500`}
                            >
                                <td >{orderItem[0]}</td>
                                <td >{orderItem[1]} </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
            
        </div>
        <div className={classes.actions}>
                <button type='back' onClick={() => { navigate('/orders')}}>Back to orders</button>
            </div>
        </>
       
    );}
