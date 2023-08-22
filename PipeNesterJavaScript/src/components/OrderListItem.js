import classes from './OrderList.module.css';

export default function OrderListItem(props){
    
    return (
        <>
        <div className="flex">
            <h1 className={classes.h1}>Order no: </h1> 
            <h2 className={classes.h2}>{props.order.orderNumber}</h2>
            </div>
            <h1 className={classes.h1}>Order summary: </h1> 
            <ul>
                {props.order.order.map((o)=>{
                return <li className={classes.h2}>{o}</li>;})}
            </ul> 
        </>           
    );

};