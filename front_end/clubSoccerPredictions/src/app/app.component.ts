import { Component } from '@angular/core';
import { Users } from './global_interfaces/users';
import UsersJson from './users.json';
  


@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {
  title = 'clubSoccerPredictions';
  Users: Users[] = UsersJson;
  constructor(){
    console.log(this.Users);
  }
}
