
function UserGreeting(props) {
    return <h1>Welcome back!</h1>;
}

let Greeting = props => React.createElement('div', {className: "commentBox"}, "Hello, world!")
let Greeting = props => {
    console.log('effect')
    return props.test ? <div className="commentBox">{props.comment}</div> : null;
    return <if test={props.test} left={<div className="commentBox">{props.comment}</div>} right={null}></if>
}

function GuestGreeting(props) {
    return <h1>Please sign up.</h1>;
}

let Greeting = props => {
    <Wrapper key={0}>
        <div><PromptButtonLogo isLoggedIn={props.isLoggedIn}/></div>
        {isLoggedIn ? <UserGreeting props={props}/> : <GuestGreeting props={props}/>}
    </Wrapper>;
}

let Greeting =


function Greeting(props) {
    return ""}

ReactDOM.render(
    // Try changing to isLoggedIn={true}:
    <Greeting isLoggedIn={false} />,
    document.getElementById('root')
);

