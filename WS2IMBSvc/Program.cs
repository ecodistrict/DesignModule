using System;
using System.Collections.Generic;
using System.Linq;
using System.ServiceProcess;
using System.Text;
using System.Threading.Tasks;

using System.Diagnostics;

namespace WS2IMBSvc
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        static void Main(string[] args)
        {

            using (var service = new WS2IMBService())
            {
                //if (Debugger.IsAttached || (args.Length > 0 && args[0].ToLower().Equals("/console")))
                if (Environment.UserInteractive)
                {
                    service.DoStart(args);
                    Boolean quit = false;
                    while (!quit)
                    {
                        Console.WriteLine("");
                        Console.WriteLine("Press Q to stop program. Press U to send update.");
                        ConsoleKeyInfo ki = Console.ReadKey();
                        switch (ki.Key)
                        {
                            case ConsoleKey.Q:
                                {
                                    quit = true;
                                    break;
                                }
                            /*
                            case ConsoleKey.U:
                                {
                                    //service.DebugUpdate();
                                    break;
                                }
                            */
                        }
                    }
                    Console.WriteLine("Service stopping");
                    service.DoStop();
                    Console.WriteLine("Service stopped");
                }
                else
                {
                    ServiceBase.Run(service);
                }
            }

        }
    }
}
