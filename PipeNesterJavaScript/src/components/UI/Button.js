import classes from './Button.module.css'

export default function Button(props){
    return <btn className={classes[success]}>{props.children}</btn> 
};